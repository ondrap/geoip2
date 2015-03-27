{-# LANGUAGE MultiWayIf #-}
module Data.GeoIP2.Fields where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.Bits ((.&.), shift)
import Data.Word
import Data.Binary
import Data.Binary.Get
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Text.Encoding    (decodeUtf8)
import Data.Int
import Data.Maybe (fromMaybe)

import Debug.Trace

data GeoField =
    DataPointer Int64
  | DataString T.Text
  | DataDouble Double
  | DataInt Int64
  | DataWord Word64
  | DataMap (Map.Map GeoField GeoField)
  | DataArray [GeoField]
  | DataBool Bool
  deriving (Eq, Ord, Show)

class GeoConvertable a where
  cvtGeo :: GeoField -> Maybe a

(.:?) :: GeoConvertable a => Map.Map GeoField GeoField -> T.Text -> Maybe a
geo .:? name = Map.lookup (DataString name) geo >>= cvtGeo

(.:) :: GeoConvertable a => Map.Map GeoField GeoField  -> T.Text -> a
geo .: key = fromMaybe (error "Cannot find key.") (geo .:? key)

instance GeoConvertable T.Text where
  cvtGeo (DataString txt) = Just txt
  cvtGeo _ = Nothing
instance GeoConvertable Int where
  cvtGeo (DataInt i) = Just $ fromIntegral i
  cvtGeo (DataWord w) = Just $ fromIntegral w
  cvtGeo _ = Nothing
instance GeoConvertable Int64 where
  cvtGeo (DataInt i) = Just $ fromIntegral i
  cvtGeo (DataWord w) = Just $ fromIntegral w
  cvtGeo _ = Nothing
instance GeoConvertable Word where
  cvtGeo (DataInt i) = Just $ fromIntegral i
  cvtGeo (DataWord w) = Just $ fromIntegral w
  cvtGeo _ = Nothing
instance GeoConvertable a => GeoConvertable [a] where
  cvtGeo (DataArray arr) = mapM cvtGeo arr
  cvtGeo _ = Nothing
instance GeoConvertable Bool where
  cvtGeo (DataBool b) = Just b
  cvtGeo _ = Nothing

mdebug str arg = trace (str ++ ": " ++ show arg) arg

-- | Parse number of given length
parseNumber :: Num a => Int64 -> Get a
parseNumber fsize = do
  bytes <- map fromIntegral <$> replicateM (fromIntegral fsize) getWord8
  return $ foldl (\acc new -> new + 256 * acc) 0 bytes

instance Binary GeoField where
  put = undefined
  get = do
    control <- getWord8
    ftype <-  if | control .&. 0xe0 == 0 -> (+7) <$> getWord8
                 | otherwise -> return $ control `shift` (-5)
    let _fsize = fromIntegral $ control .&. 0x1f :: Int64
    fsize <- if
            | ftype == 1 -> do
                  let _ss = _fsize `shift` (-3)
                      _vval = fromIntegral $ _fsize .&. 0x7
                  case _ss of
                    0 -> ((_vval `shift` 8) +) <$> parseNumber 1
                    1 -> ((2048 + (_vval `shift` 16)) +) <$> parseNumber 2
                    2 -> ((526336 + (_vval `shift` 24)) +) <$> parseNumber 3
                    3 -> parseNumber 4
                    _ -> error "Cannot happen"
            | _fsize < 29  -> return _fsize
            | _fsize == 29 -> (29+) <$> parseNumber 1
            | _fsize == 30 -> (285+) <$> parseNumber 2
            | _fsize == 31 ->  (65821+) <$> parseNumber 3

    case ftype of
        1 -> return $ DataPointer fsize
        2 -> DataString . decodeUtf8 <$> getByteString (fromIntegral fsize)
        3 -> do
            number <- mdebug "double" <$> get :: Get Double
            return $ DataDouble number
        5 -> DataWord <$> parseNumber fsize
        6 -> DataWord <$> parseNumber fsize
        7 -> do
            pairs <- replicateM (fromIntegral fsize) $ do
                    key <- get
                    val <- get
                    return (key, val)
            return $ DataMap (Map.fromList pairs)
        9 -> DataWord <$> parseNumber fsize
        11 -> DataArray <$> replicateM (fromIntegral fsize) get
        _ -> error ("Cannot parse: Type: " ++ show ftype ++ ", Size: " ++ show fsize)
