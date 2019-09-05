{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

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
  -- * Internals
  , GeoField, GeoFieldT(..)
  , findRawGeoData
  -- * Lenses 
  , _DataString, _DataDouble, _DataInt, _DataWord
  , _DataMap, _DataArray, _DataBool, _DataUnknown
) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative    ((<$>), (<*>))
#endif

import           Control.Monad          (unless, when)
import qualified Data.ByteString        as BS
import           Data.Int
import           Data.IP                (IP (..), ipv4ToIPv6)
import           Data.Maybe             (mapMaybe)
import           Data.Serialize
import qualified Data.Text              as T
import           System.IO.MMap
import           Control.Lens           (ix, (^?), _Just, to, (^..), Traversal', Fold, prism')
import           Control.Exception      (throwIO, Exception)

import           Data.GeoIP2.Fields
import           Data.GeoIP2.SearchTree

-- | Address type stored in database
data GeoIP = GeoIPv6 | GeoIPv4 deriving (Eq, Show)

data DecodeException = DecodeException String
  deriving (Show)
instance Exception DecodeException

throwErr :: String -> IO a
throwErr = throwIO . DecodeException

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
    openGeoDBBS bsmem

openGeoDBBS :: BS.ByteString -> IO GeoDB
openGeoDBBS bsmem = do
    hdr <- either throwErr return $ decode (getHeaderBytes bsmem)
    when (hdr ^? key "binary_format_major_version" . geoNum /= (Just 2 :: Maybe Int)) $ 
      throwErr "Unsupported database version, only v2 supported."
    unless (hdr ^? key "record_size" . geoNum `elem` (Just <$> [24, 28, 32 :: Int])) $
      throwErr "Record size not supported."

    let res = GeoDB bsmem <$> hdr ^? key "database_type" . _DataString
                          <*> pure (hdr ^.. key "languages" . _DataArray . traverse . _DataString)
                          <*> hdr ^? key "node_count" . geoNum
                          <*> hdr ^? key "record_size" . geoNum
                          <*> hdr ^? key "ip_version" . toVersion
                          <*> pure (hdr ^? key "descritpion" . key "en" . _DataString)
    maybe (throwErr "Error decoding header") return res
  where
    toVersion = geoNum . prism' pfrom pto
      where
        pfrom :: GeoIP -> Int
        pfrom GeoIPv4 = 4
        pfrom GeoIPv6 = 6
        pto 4 = Just GeoIPv4
        pto 6 = Just GeoIPv6
        pto _ = Nothing

rawGeoData :: GeoDB -> IP -> Either String GeoField
rawGeoData geodb addr = do
  bits <- coerceAddr
  offset <- getDataOffset (geoMem geodb, geoDbNodeCount geodb, geoDbRecordSize geodb) bits
  strictDataAt offset
  where
    dataSectionStart = (geoDbRecordSize geodb `div` 4) * fromIntegral (geoDbNodeCount geodb) + 16
    dataSection = BS.drop dataSectionStart (geoMem geodb)
    
    strictDataAt :: Int64 -> Either String GeoField
    strictDataAt offset = do
      raw <- decode (BS.drop (fromIntegral offset) dataSection)
      traversePtr (strictDataAt . fromIntegral) raw
  
    coerceAddr
      | (IPv4 _) <- addr, GeoIPv4 <- geoDbAddrType geodb = return $ ipToBits addr
      | (IPv6 _) <- addr, GeoIPv6 <- geoDbAddrType geodb = return $ ipToBits addr
      | (IPv4 addrv4) <- addr, GeoIPv6 <- geoDbAddrType geodb = return $ ipToBits $ IPv6 (ipv4ToIPv6 addrv4)
      | otherwise = Left "Cannot search IPv6 address in IPv4 database"

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
  , locationAccuracy :: Maybe Int
} deriving (Show, Eq)

-- | Search GeoIP database
findGeoData ::
     GeoDB   -- ^ Db handle
  -> T.Text  -- ^ Language code (e.g. "en")
  -> IP      -- ^ IP address to search
  -> Either String GeoResult -- ^ Result, if something is found
findGeoData geodb lang ip = do
  res <- rawGeoData geodb ip
  let subdivmap = res ^.. key "subdivisions" . _DataArray . traverse
      subdivs = mapMaybe (\s -> (,) <$> s ^? key "iso_code"  . _DataString
                                    <*> s ^? key "names" . key lang . _DataString) subdivmap

  return $ GeoResult (res ^? key "continent" . key "names" . key lang . _DataString)
                     (res ^? key "continent" . key "code" . _DataString)
                     (res ^? key "country" . key "iso_code" . _DataString)
                     (res ^? key "country" . key "names" . key lang . _DataString)
                     (Location <$> res ^? key "location" . key "latitude" . _DataDouble
                              <*> res ^? key "location" . key "longitude" . _DataDouble
                              <*> res ^? key "location" . key "time_zone" . _DataString
                              <*> pure (res ^? key "location" . key "accuracy_radius" . geoNum))
                     (res ^? key "city" . key "names" . key lang . _DataString)
                     (res ^? key "postal" . key "code" . _DataString)
                     subdivs

key :: T.Text -> Traversal' GeoField GeoField
key k = _DataMap . ix (DataString k)

geoNum :: Num b => Fold GeoField b
geoNum = to fromNum . _Just
  where
    fromNum (DataInt x) = Just (fromIntegral x)
    fromNum (DataWord x) = Just (fromIntegral x)
    fromNum _ = Nothing

-- | Search GeoIP database and return complete unparsed data
findRawGeoData :: GeoDB -> IP -> Either String GeoField
findRawGeoData goedb ip = rawGeoData goedb ip
