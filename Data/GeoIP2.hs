{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Data.GeoIP2 (
  makeGeoDB
) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM, foldM)
import System.IO.Posix.MMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Bits ((.&.), (.|.), shift, rotate)
import Data.Word
import Data.Binary
import Data.Binary.Get
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Text.Encoding    (decodeUtf8)
import Data.Int

data GeoDB = GeoDB {
	 mem :: BL.ByteString
}

data GeoField =
    DataPointer Int
  | DataString T.Text
  | DataDouble Double
  | DataInt Int64
  | DataWord Word64
  | DataMap (Map.Map GeoField GeoField)
  | DataArray [GeoField]
  | DataBool Bool
  deriving (Eq, Ord, Show)



instance Binary GeoField where
  put = undefined
  get = do
    control <- getWord8
    let fsize = fromIntegral $ control .&. 0x1f :: Int
    ftype <- if | control .&. 0xe0 == 0 -> (+7) <$> getWord8
                | otherwise -> return $ control `shift` (-5)
    case ftype of
        2 -> DataString . decodeUtf8 <$> getByteString fsize
        5 -> parseUnsigned fsize
        6 -> parseUnsigned fsize
        7 -> do
            pairs <- replicateM fsize $ do
                    key <- get
                    val <- get
                    return (key, val)
            return $ DataMap (Map.fromList pairs)
        9 -> parseUnsigned fsize
        11 -> DataArray <$> replicateM fsize get
        _ -> error ("Cannot parse: Type: " ++ show ftype ++ ", Size: " ++ show fsize)
      where
        parseUnsigned fsize = do
          bytes <- map fromIntegral <$> replicateM fsize getWord8 :: Get [Word64]
          return $ DataWord $ foldl (\acc new -> new + (acc `shift` 8)) 0 bytes

getHeaderBytes = lastsubstring "\xab\xcd\xefMaxMind.com"
  where
    lastsubstring pattern string =
        case BS.breakSubstring pattern string of
            (res, "") -> res
            (_, rest) -> lastsubstring pattern (BS.drop (BS.length pattern) rest)

makeGeoDB :: FilePath -> IO (Either String GeoDB)
makeGeoDB geoFile = do
    memo <- unsafeMMapFile geoFile
    let hdr = BL.fromChunks [getHeaderBytes memo]
    print hdr
    let hdrdata = decode hdr :: GeoField
    print hdrdata
    return $ Left "test"



main :: IO ()
main = do
  res <- makeGeoDB "GeoLite2-Country.mmdb"
  return ()
