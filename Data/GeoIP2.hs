{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.GeoIP2 (
  -- * Library description
  -- |
  -- A haskell library for reading MaxMind's GeoIP version 2 files.
  -- It supports both IPv4 and IPv6 addresses. When a match is found, it
  -- is parsed and a simplified structure is returned. If you want to access
  -- other fields than those that are exposed, it is internally possible.
  --
  -- The database is mmapped upon opening, all querying can be later
  -- performed purely without IO monad.

  -- * Opening the database
    GeoDB
  , openGeoDB, openGeoDBBS
  , geoDbLanguages, geoDbType, geoDbDescription
  , geoDbAddrType, GeoIP(..)
  -- * Querying the database
  , findGeoData
  , GeoResult(..)
  , Location(..)
) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative    ((<$>), (<*>))
#endif

import           Control.Monad          (unless, when)
import qualified Data.ByteString        as BS
import           Data.Int
import           Data.IP                (IP (..), ipv4ToIPv6)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe, mapMaybe)
import           Data.Serialize
import qualified Data.Text              as T
import           System.IO.MMap

import           Data.GeoIP2.Fields
import           Data.GeoIP2.SearchTree

-- | Address type stored in database
data GeoIP = GeoIPv6 | GeoIPv4 deriving (Eq, Show)

-- | Handle for search operations
data GeoDB = GeoDB {
   geoMem           :: BS.ByteString
 , geoDbType        :: T.Text         -- ^ String that indicates the structure of each data record associated with an IP address
 , geoDbLanguages   :: [T.Text]  -- ^ Languages supported in database
 , geoDbNodeCount   :: Int64
 , geoDbRecordSize  :: Int
 , geoDbAddrType    :: GeoIP -- ^ Type of address (IPv4/IPv6) stored in a database
 , geoDbDescription :: Maybe T.Text -- ^ Description of a database in english
}

getHeaderBytes :: BS.ByteString -> BS.ByteString
getHeaderBytes = lastsubstring "\xab\xcd\xefMaxMind.com"
  where
    lastsubstring pattern string =
        case BS.breakSubstring pattern string of
            (res, "") -> res
            (_, rest) -> lastsubstring pattern (BS.drop (BS.length pattern) rest)

-- | Open database, mmap it into memory, parse header and return a handle for search operations
openGeoDB :: FilePath -> IO GeoDB
openGeoDB geoFile = do
    bsmem <- mmapFileByteString geoFile Nothing
    parseGeoDB bsmem

-- | Open database from a bytestring, parse header and return a handle for search operations
openGeoDBBS :: BS.ByteString -> IO GeoDB
openGeoDBBS  = parseGeoDB

parseGeoDB :: BS.ByteString -> IO GeoDB
parseGeoDB bsmem = do
    DataMap hdr <- either error return $ decode (getHeaderBytes bsmem)
    when (hdr .: "binary_format_major_version" /= (2 :: Int)) $ error "Unsupported database version, only v2 supported."
    unless (hdr .: "record_size" `elem` [24, 28, 32 :: Int]) $ error "Record size not supported."
    return $ GeoDB bsmem (hdr .: "database_type")
                       (fromMaybe [] $ hdr .:? "languages")
                       (hdr .: "node_count") (hdr .: "record_size")
                       (if (hdr .: "ip_version") == (4 :: Int) then GeoIPv4 else GeoIPv6)
                       (hdr .:? "description" ..? "en")

rawGeoData :: GeoDB -> IP -> Either String GeoField
rawGeoData geodb addr = do
  bits <- coerceAddr
  offset <- fromIntegral <$> getDataOffset (geoMem geodb, geoDbNodeCount geodb, geoDbRecordSize geodb) bits
  basedata <- dataAt offset
  resolvePointers basedata
  where
    dataSectionStart = (geoDbRecordSize geodb `div` 4) * fromIntegral (geoDbNodeCount geodb) + 16
    -- Add caching
    dataAt offset =  case decode (BS.drop (offset + dataSectionStart) (geoMem geodb)) of
                          Right res -> return res
                          Left err -> Left err
    coerceAddr
      | (IPv4 _) <- addr, GeoIPv4 <- geoDbAddrType geodb = return $ ipToBits addr
      | (IPv6 _) <- addr, GeoIPv6 <- geoDbAddrType geodb = return $ ipToBits addr
      | (IPv4 addrv4) <- addr, GeoIPv6 <- geoDbAddrType geodb = return $ ipToBits $ IPv6 (ipv4ToIPv6 addrv4)
      | otherwise = Left "Cannot search IPv6 address in IPv4 database"
    resolvePointers (DataPointer ptr) = dataAt (fromIntegral ptr) >>= resolvePointers -- TODO - limit recursion?
    resolvePointers (DataMap obj) = DataMap . Map.fromList <$> mapM resolveTuple (Map.toList obj)
    resolvePointers (DataArray arr) = DataArray <$> mapM resolvePointers arr
    resolvePointers x = return x
    resolveTuple (a,b) = (,) <$> resolvePointers a <*> resolvePointers b

-- | Result of a search query
data GeoResult = GeoResult {
    geoContinent     :: Maybe T.Text
  , geoContinentCode :: Maybe T.Text
  , geoCountryISO    :: Maybe T.Text
  , geoCountry       :: Maybe T.Text
  , geoLocation      :: Maybe Location
  , geoCity          :: Maybe T.Text
  , geoPostalCode    :: Maybe T.Text
  , geoSubdivisions  :: [(T.Text, T.Text)]
} deriving (Show, Eq)

data Location = Location {
    locationLatitude :: Double
  , locationLongitude :: Double
  , locationTimezone :: T.Text
  , locationAccuracy :: Int
} deriving (Show, Eq)

-- | Search GeoIP database
findGeoData ::
     GeoDB   -- ^ Db handle
  -> T.Text  -- ^ Language code (e.g. "en")
  -> IP      -- ^ IP address to search
  -> Either String GeoResult -- ^ Result, if something is found
findGeoData geodb lang ip = do
  res <- rawGeoData geodb ip >>= asMap
  let subdivmap = res .:? "subdivisions" :: Maybe [Map.Map GeoField GeoField]
      subdivs = mapMaybe (\s -> (,) <$> s .:? "iso_code" <*> s .:? "names" ..? lang) <$> subdivmap

  return $ GeoResult (res .:? "continent" ..? "names" ..? lang)
                     (res .:? "continent" ..? "code")
                     (res .:? "country" ..? "iso_code")
                     (res .:? "country" ..? "names" ..? lang)
                     (Location <$> res .:? "location" ..? "latitude"
                        <*> res .:? "location" ..? "longitude"
                        <*> res .:? "location" ..? "time_zone"
                        <*> res .:? "location" ..? "accuracy_radius")
                     (res .:? "city" ..? "names" ..? lang)
                     (res .:? "postal" ..? "code")
                     (fromMaybe [] subdivs)
  where
    asMap (DataMap res) = return res
    asMap _             = Left "rawGeoData returned something else than DataMap"
