{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Data.GeoIP2 (
    makeGeoDB
  , findGeoData
) where

import Control.Monad (when, unless)
import System.IO.Posix.MMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Binary
import qualified Data.Text as T
import Data.IP (IP(..), ipv4ToIPv6)
import Control.Applicative ((<$>))
import Control.Monad (mapM)
import qualified Data.Map as Map

import Data.GeoIP2.Fields
import Data.GeoIP2.SearchTree

data GeoIP = GeoIPv6 | GeoIPv4 deriving (Eq, Show)

data GeoDB = GeoDB {
	 geoMem :: BL.ByteString
 , geoDatabaseType :: T.Text
 , geoLanguages :: [T.Text]
 , geoNodeCount :: Int64
 , geoRecordSize :: Int
 , geoAddrType :: GeoIP
}

getHeaderBytes :: BS.ByteString -> BS.ByteString
getHeaderBytes = mdebug "test" <$> lastsubstring "\xab\xcd\xefMaxMind.com"
  where
    lastsubstring pattern string =
        case BS.breakSubstring pattern string of
            (res, "") -> res
            (_, rest) -> lastsubstring pattern (BS.drop (BS.length pattern) rest)

makeGeoDB :: FilePath -> IO GeoDB
makeGeoDB geoFile = do
    bsmem <- unsafeMMapFile geoFile
    let (DataMap hdr) = mdebug "hdr" $ decode (BL.fromChunks [getHeaderBytes bsmem])
        mem = BL.fromChunks [bsmem]
    when (hdr .: "binary_format_major_version" /= (2 :: Int)) $ error "Unsupported database version, only v2 supported."
    unless (hdr .: "record_size" `elem` [24, 28, 32 :: Int]) $ error "Record size not supported."
    return $ GeoDB mem (hdr .: "database_type") (hdr .: "languages")
                       (hdr .: "node_count") (hdr .: "record_size")
                       (if (hdr .: "ip_version") == (4 :: Int) then GeoIPv4 else GeoIPv6)



findGeoData :: GeoDB -> IP -> Maybe GeoField
findGeoData geodb addr = do
  bits <- coerceAddr
  offset <- mdebug "offset" <$> getDataOffset (geoMem geodb, geoNodeCount geodb, geoRecordSize geodb) bits
  let basedata = dataAt offset
  return $ resolvePointers basedata
  where
    dataSectionStart = fromIntegral (geoRecordSize geodb `div` 4) * geoNodeCount geodb + 16
    dataAt offset = decode (BL.drop (offset + dataSectionStart) (geoMem geodb))
    coerceAddr
      | (IPv4 _) <- addr, GeoIPv4 <- geoAddrType geodb = return $ ipToBits addr
      | (IPv6 _) <- addr, GeoIPv6 <- geoAddrType geodb = return $ ipToBits addr
      | (IPv4 addrv4) <- addr, GeoIPv6 <- geoAddrType geodb = return $ ipToBits $ IPv6 (ipv4ToIPv6 addrv4)
      | otherwise = Nothing
    resolvePointers (DataPointer ptr) = resolvePointers $ dataAt ptr
    resolvePointers (DataMap obj) = DataMap $ Map.fromList $ map resolveTuple (Map.toList obj)
    resolvePointers (DataArray arr) = DataArray $ map resolvePointers arr
    resolvePointers x = x
    resolveTuple (a,b) =
        let r1 = resolvePointers a
            r2 = resolvePointers b
        in (r1, r2)

main :: IO ()
main = do
  geodb <- makeGeoDB "GeoLite2-City.mmdb"
  print (geoAddrType geodb)

  let addr = IPv4 "212.58.246.91"
  print $ findGeoData geodb addr
