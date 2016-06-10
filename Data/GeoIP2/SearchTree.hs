{-# OPTIONS_HADDOCK hide #-}

module Data.GeoIP2.SearchTree where

import           Data.Bits       (shift, testBit, (.&.), (.|.))
import qualified Data.ByteString as BS
import           Data.Int
import           Data.IP         (IP (..), fromIPv4, fromIPv6b)
import           Data.Word

-- | Convert byte to list of bits starting from the most significant one
byteToBits :: Int -> [Bool]
byteToBits b = map (testBit b) [7,6..0]

-- | Convert IP address to bits
ipToBits :: IP -> [Bool]
ipToBits (IPv4 addr) = concatMap byteToBits (fromIPv4 addr)
ipToBits (IPv6 addr) = concatMap byteToBits (fromIPv6b addr)

-- | Read node (2 records) given the index of a node
readNode :: BS.ByteString -> Int -> Int64 -> (Int64, Int64)
readNode mem recordbits index =
  let
    bytecount = fromIntegral $ recordbits `div` 4
    bytes = BS.take (fromIntegral bytecount) $ BS.drop (fromIntegral $ index * bytecount) mem
    num = BS.foldl' (\acc new -> fromIntegral new + 256 * acc) 0 bytes :: Word64
    -- 28 bits has a strange record format
    left28 = num `shift` (-32) .|. (num .&. 0xf0000000) `shift` (-4)
  in case recordbits of
      28 -> (fromIntegral left28, fromIntegral (num .&. ((1 `shift` recordbits) - 1)))
      _  -> (fromIntegral (num `shift` negate recordbits), fromIntegral (num .&. ((1 `shift` recordbits) - 1)))

-- | Get offset in the Data Section
getDataOffset :: (BS.ByteString, Int64, Int) -> [Bool] -> Either String Int64
getDataOffset (mem, nodeCount, recordSize) startbits =
  getnode startbits 0
  where
    getnode _ index
      | index == nodeCount = Left "Information for address does not exist."
      | index > nodeCount = return $ index - nodeCount - 16
    getnode [] _ = Left "IP address too short????"
    getnode (bit:rest) index = getnode rest nextOffset
      where
        (left, right) = readNode mem recordSize index
        nextOffset = if bit then right else left
