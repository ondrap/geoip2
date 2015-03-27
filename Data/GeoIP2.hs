{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Data.GeoIP2 (
    openGeoDB
  , findGeoData
  , GeoDB
  , geoDbLanguages, geoDbType, geoDbDescription
  , GeoResult
  , geoContinent, geoCountryISO, geoCountry, geoCity, geoSubdivisions, geoLocation
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

data GeoIP = GeoIPv6 | GeoIPv4 deriving (Eq, Show)

data GeoDB = GeoDB {
	 geoMem :: BL.ByteString
 , geoDbType :: T.Text
 , geoDbLanguages :: [T.Text]
 , geoDbNodeCount :: Int64
 , geoDbRecordSize :: Int
 , geoDbAddrType :: GeoIP
 , geoDbDescription :: Maybe T.Text
}

getHeaderBytes :: BS.ByteString -> BS.ByteString
getHeaderBytes = lastsubstring "\xab\xcd\xefMaxMind.com"
  where
    lastsubstring pattern string =
        case BS.breakSubstring pattern string of
            (res, "") -> res
            (_, rest) -> lastsubstring pattern (BS.drop (BS.length pattern) rest)

openGeoDB :: FilePath -> IO GeoDB
openGeoDB geoFile = do
    bsmem <- unsafeMMapFile geoFile
    let (DataMap hdr) = decode (BL.fromChunks [getHeaderBytes bsmem])
        mem = BL.fromChunks [bsmem]
    when (hdr .: "binary_format_major_version" /= (2 :: Int)) $ error "Unsupported database version, only v2 supported."
    unless (hdr .: "record_size" `elem` [24, 28, 32 :: Int]) $ error "Record size not supported."
    return $ GeoDB mem (hdr .: "database_type") (hdr .: "languages")
                       (hdr .: "node_count") (hdr .: "record_size")
                       (if (hdr .: "ip_version") == (4 :: Int) then GeoIPv4 else GeoIPv6)
                       (hdr .:? "description" ..? "en")



rawGeoData :: GeoDB -> IP -> Maybe GeoField
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
      | otherwise = Nothing
    resolvePointers (DataPointer ptr) = resolvePointers $ dataAt ptr
    resolvePointers (DataMap obj) = DataMap $ Map.fromList $ map resolveTuple (Map.toList obj)
    resolvePointers (DataArray arr) = DataArray $ map resolvePointers arr
    resolvePointers x = x
    resolveTuple (a,b) =
        let r1 = resolvePointers a
            r2 = resolvePointers b
        in (r1, r2)

data GeoResult = GeoResult {
    geoContinent :: Maybe T.Text
  , geoCountryISO :: Maybe T.Text
  , geoCountry :: Maybe T.Text
  , geoLocation :: Maybe (Double, Double)
  , geoCity :: Maybe T.Text
  , geoSubdivisions :: [(T.Text, T.Text)]
} deriving (Show, Eq)

findGeoData :: GeoDB -> T.Text -> IP -> Maybe GeoResult
findGeoData geodb lang ip = do
  (DataMap res) <- rawGeoData geodb ip
  let subdivmap = res .:? "subdivisions" :: Maybe [Map.Map GeoField GeoField]
      subdivs = mapMaybe (\s -> (,) <$> s .:? "iso_code" <*> s .:? "names" ..? lang) <$> subdivmap

  return $ GeoResult (res .:? "continent" ..? "names" ..? lang)
                     (res .:? "country" ..? "iso_code")
                     (res .:? "country" ..? "names" ..? lang)
                     ((,) <$> res .:? "location" ..? "latitude" <*> res .:? "location" ..? "longitude")
                     (res .:? "city" ..? "names" ..? lang)
                     (fromMaybe [] subdivs)
