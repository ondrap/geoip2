{-# OPTIONS_HADDOCK hide #-}

module Data.GeoIP2.SearchTree where

import           Data.Binary
import           Data.Bits            (shift, testBit, (.&.), (.|.))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.IP              (IP (..), fromIPv4, fromIPv6b)

-- | Convert byte to list of bits starting from the most significant one
byteToBits :: Int -> [Bool]
byteToBits b = map (testBit b) [7,6..0]

-- | Convert IP address to bits
ipToBits :: IP -> [Bool]
ipToBits (IPv4 addr) = concatMap byteToBits (fromIPv4 addr)
ipToBits (IPv6 addr) = concatMap byteToBits (fromIPv6b addr)

-- | Read node (2 records) given the index of a node
readNode :: BL.ByteString -> Int -> Int64 -> (Int64, Int64)
readNode mem recordbits index =
  let
    bytecount = fromIntegral $ recordbits `div` 4
    bytes = BL.take (fromIntegral bytecount) $ BL.drop (fromIntegral $ index * bytecount) mem
    numbers = concatMap BS.unpack (BL.toChunks bytes) :: [Word8]
    makenum = foldl (\acc new -> fromIntegral new + 256 * acc) 0 :: [Word8] -> Word64
    num = makenum numbers
    -- 28 bits has a strange record format
    left28 = num `shift` (-32) .|. (num .&. 0xf0000000)
  in case recordbits of
      28 -> (fromIntegral left28, fromIntegral (num .&. ((1 `shift` recordbits) - 1)))
      _  -> (fromIntegral (num `shift` negate recordbits), fromIntegral (num .&. ((1 `shift` recordbits) - 1)))

-- | Get offset in the Data Section
getDataOffset :: (BL.ByteString, Int64, Int) -> [Bool] -> Maybe Int64
getDataOffset (mem, nodeCount, recordSize) startbits =
  getnode startbits 0
  where
    getnode _ index
      | index == nodeCount = Nothing
      | index > nodeCount = Just $ index - nodeCount - 16
    getnode [] _ = Nothing
    getnode (bit:rest) index = getnode rest nextOffset
      where
        (left, right) = readNode mem recordSize index
        nextOffset = if bit then right else left
