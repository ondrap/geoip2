{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

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
  , openGeoDB
  , geoDbLanguages, geoDbType, geoDbDescription, geoDbAddrType
  -- * Querying the database
  , findGeoData
  , GeoResult(..)
) where

import Control.Monad (when, unless)
import System.IO.Posix.MMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Binary
import qualified Data.Text as T
import Data.IP (IP(..), ipv4ToIPv6)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)

import Data.GeoIP2.Fields
import Data.GeoIP2.SearchTree

-- | Address type stored in database
data GeoIP = GeoIPv6 | GeoIPv4 deriving (Eq, Show)

-- | Handle for search operations
data GeoDB = GeoDB {
	 geoMem :: BL.ByteString
 , geoDbType :: T.Text         -- ^ String that indicates the structure of each data record associated with an IP address
 , geoDbLanguages :: [T.Text]  -- ^ Languages supported in database
 , geoDbNodeCount :: Int64
 , geoDbRecordSize :: Int
 , geoDbAddrType :: GeoIP -- ^ Type of address (IPv4/IPv6) stored in a database
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
    bsmem <- unsafeMMapFile geoFile
    let (DataMap hdr) = decode (BL.fromChunks [getHeaderBytes bsmem])
        mem = BL.fromChunks [bsmem]
    when (hdr .: "binary_format_major_version" /= (2 :: Int)) $ error "Unsupported database version, only v2 supported."
    unless (hdr .: "record_size" `elem` [24, 28, 32 :: Int]) $ error "Record size not supported."
    return $ GeoDB mem (hdr .: "database_type")
                       (fromMaybe [] $ hdr .:? "languages")
                       (hdr .: "node_count") (hdr .: "record_size")
                       (if (hdr .: "ip_version") == (4 :: Int) then GeoIPv4 else GeoIPv6)
                       (hdr .:? "description" ..? "en")



rawGeoData :: Monad m => GeoDB -> IP -> m GeoField
rawGeoData geodb addr = do
  bits <- coerceAddr
  offset <- getDataOffset (geoMem geodb, geoDbNodeCount geodb, geoDbRecordSize geodb) bits
  let basedata = dataAt offset
  return $ resolvePointers basedata
  where
    dataSectionStart = fromIntegral (geoDbRecordSize geodb `div` 4) * geoDbNodeCount geodb + 16
    dataAt offset = decode (BL.drop (offset + dataSectionStart) (geoMem geodb))
    coerceAddr
      | (IPv4 _) <- addr, GeoIPv4 <- geoDbAddrType geodb = return $ ipToBits addr
      | (IPv6 _) <- addr, GeoIPv6 <- geoDbAddrType geodb = return $ ipToBits addr
      | (IPv4 addrv4) <- addr, GeoIPv6 <- geoDbAddrType geodb = return $ ipToBits $ IPv6 (ipv4ToIPv6 addrv4)
      | otherwise = fail "Cannot search IPv6 address in IPv4 database"
    resolvePointers (DataPointer ptr) = resolvePointers $ dataAt ptr
    resolvePointers (DataMap obj) = DataMap $ Map.fromList $ map resolveTuple (Map.toList obj)
    resolvePointers (DataArray arr) = DataArray $ map resolvePointers arr
    resolvePointers x = x
    resolveTuple (a,b) =
        let r1 = resolvePointers a
            r2 = resolvePointers b
        in (r1, r2)

-- | Result of a search query
data GeoResult = GeoResult {
    geoContinent :: Maybe T.Text
  , geoContinentCode :: Maybe T.Text
  , geoCountryISO :: Maybe T.Text
  , geoCountry :: Maybe T.Text
  , geoLocation :: Maybe (Double, Double)
  , geoCity :: Maybe T.Text
  , geoSubdivisions :: [(T.Text, T.Text)]
} deriving (Show, Eq)

-- | Search GeoIP database, monadic version (e.g. use with Maybe or Either)
findGeoData :: Monad m =>
     GeoDB   -- ^ Db handle
  -> T.Text  -- ^ Language code (e.g. "en")
  -> IP      -- ^ IP address to search
  -> m GeoResult -- ^ Result, if something is found
findGeoData geodb lang ip = do
  (DataMap res) <- rawGeoData geodb ip
  let subdivmap = res .:? "subdivisions" :: Maybe [Map.Map GeoField GeoField]
      subdivs = mapMaybe (\s -> (,) <$> s .:? "iso_code" <*> s .:? "names" ..? lang) <$> subdivmap

  return $ GeoResult (res .:? "continent" ..? "names" ..? lang)
                     (res .:? "continent" ..? "code")
                     (res .:? "country" ..? "iso_code")
                     (res .:? "country" ..? "names" ..? lang)
                     ((,) <$> res .:? "location" ..? "latitude" <*> res .:? "location" ..? "longitude")
                     (res .:? "city" ..? "names" ..? lang)
                     (fromMaybe [] subdivs)
