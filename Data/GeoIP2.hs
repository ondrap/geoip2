{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Data.GeoIP2 (
  makeGeoDB
) where

import Control.Monad (when, unless)
import System.IO.Posix.MMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Int
import Data.Binary
import qualified Data.Text as T
import Data.Bits (testBit, shift, (.&.))
import Data.IP

import Data.GeoIP2.Fields

import Debug.Trace

data GeoIP = GeoIPv6 | GeoIPv4 deriving (Eq, Show)

data GeoDB = GeoDB {
	 geoMem :: BL.ByteString
 , geoDatabaseType :: T.Text
 , geoLanguages :: [T.Text]
 , geoNodeCount :: Int64
 , geoRecordSize :: Int
 , geoAddrType :: GeoIP
}

byteToBits :: Int -> [Bool]
byteToBits b = map (testBit b) [7,6..0]

ipToBits :: IP -> [Bool]
ipToBits (IPv4 addr) = concatMap byteToBits (fromIPv4 addr)
ipToBits (IPv6 addr) = concatMap byteToBits (fromIPv6b addr)

readNode :: BL.ByteString -> Int -> Int64 -> (Int64, Int64)
-- readNode _ _ index | trace ("Reading node: " ++ show index) False = undefined
readNode mem recordbits index =
  let
    bytecount = fromIntegral $ recordbits `div` 4
    bytes = BL.take (fromIntegral bytecount) $ BL.drop (fromIntegral $ index * bytecount) mem
    numbers = concatMap BS.unpack (BL.toChunks bytes) :: [Word8]
    makenum = foldl (\acc new -> (fromIntegral new) + 256 * acc) 0 :: [Word8] -> Int64
    num = makenum numbers
  in
    (fromIntegral (num `shift` negate recordbits), fromIntegral (num .&. ((1 `shift` recordbits) - 1)))

getDataOffset :: GeoDB -> [Bool] -> Maybe Int64
getDataOffset (GeoDB {geoMem=mem, geoNodeCount=nodeCount, geoRecordSize=recordSize}) startbits =
  getnode startbits 0
  where
    getnode _ index
      | index == nodeCount = Nothing
      | index > nodeCount = Just $ index - nodeCount
    getnode [] _ = Nothing
    getnode (bit:rest) index = getnode rest nextOffset
      where
        (left, right) = readNode mem recordSize index
        nextOffset = if bit then right else left

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
    unless (hdr .: "record_size" `elem` [24, 28, 32 :: Int]) $ error "Record size not supported."
    return $ GeoDB mem (hdr .: "database_type") (hdr .: "languages")
                       (hdr .: "node_count") (hdr .: "record_size")
                       (if (hdr .: "ip_version") == (4 :: Int) then GeoIPv4 else GeoIPv6)

main :: IO ()
main = do
  geodb <- makeGeoDB "GeoLite2-Country.mmdb"
  print (geoAddrType geodb)

  let addr = "212.58.246.91" :: IPv4
      -- addr = "193.165.254.33" :: IPv4
      -- addr = "192.168.2.3" :: IPv4
      bits = ipToBits $ IPv6 (ipv4ToIPv6 addr)
      moffset = getDataOffset geodb bits
      dataSectionStart = (fromIntegral $ geoRecordSize geodb `div` 4) * geoNodeCount geodb + 16
  print moffset
  putStrLn "-------------"
  case moffset of
    Nothing -> putStrLn "Not found"
    Just offset -> do
        let item = decode (BL.drop (offset + dataSectionStart - 16) (geoMem geodb)) :: GeoField
        print item


  return ()
