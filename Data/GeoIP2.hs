{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Data.GeoIP2 (
  makeGeoDB
) where

import Control.Monad (when)
import System.IO.Posix.MMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Binary
import qualified Data.Text as T

import Data.GeoIP2.Fields

data GeoDB = GeoDB {
	 geoMem :: BL.ByteString
 , geoDatabaseType :: T.Text
 , geoLanguages :: [T.Text]
 , geoNodeCound :: Word
 , geoNodeSize :: Int
}

getHeaderBytes :: BS.ByteString -> BS.ByteString
getHeaderBytes = lastsubstring "\xab\xcd\xefMaxMind.com"
  where
    lastsubstring pattern string =
        case BS.breakSubstring pattern string of
            (res, "") -> res
            (_, rest) -> lastsubstring pattern (BS.drop (BS.length pattern) rest)

makeGeoDB :: FilePath -> IO GeoDB
makeGeoDB geoFile = do
    bsmem <- unsafeMMapFile geoFile
    let (DataMap hdr) = decode (BL.fromChunks [getHeaderBytes bsmem])
        mem = BL.fromChunks [bsmem]
    when (hdr .: "binary_format_major_version" /= (2 :: Int)) $ error "Unsupported database version, only v2 supported."

    return $ GeoDB mem (hdr .: "database_type") (hdr .: "languages") (hdr .: "node_count") (hdr .: "record_size")

main :: IO ()
main = do
  geodb <- makeGeoDB "GeoLite2-Country.mmdb"
  print (geoLanguages geodb)
  return ()
