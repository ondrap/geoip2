{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE CPP               #-}

module Data.GeoIP2.Fields where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative  ((<$>))
#endif

import           Control.Monad        (replicateM)
import           Data.Serialize
import           Data.Bits            (shift, (.&.))
import qualified Data.ByteString      as BS
import           Data.Int
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import           Data.ReinterpretCast (wordToDouble)
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8)
import           Data.Word

data GeoField =
    DataPointer Int64
  | DataString !T.Text
  | DataDouble Double
  | DataInt Int64
  | DataWord Word64
  | DataMap (Map.Map GeoField GeoField)
  | DataArray [GeoField]
  | DataBool Bool
  | DataUnknown Word8 Int64
  deriving (Eq, Ord, Show)

class GeoConvertable a where
  cvtGeo :: GeoField -> Maybe a

(..?) :: GeoConvertable a => Maybe (Map.Map GeoField GeoField) -> T.Text -> Maybe a
(Just geo) ..? name = Map.lookup (DataString name) geo >>= cvtGeo
_ ..? _ = Nothing

(.:?) :: GeoConvertable a => Map.Map GeoField GeoField -> T.Text -> Maybe a
geo .:? name = Map.lookup (DataString name) geo >>= cvtGeo

(.:) :: GeoConvertable a => Map.Map GeoField GeoField  -> T.Text -> a
geo .: key = fromMaybe (error "Cannot find key.") (geo .:? key)

instance GeoConvertable (Map.Map GeoField GeoField) where
  cvtGeo (DataMap obj) = Just obj
  cvtGeo _ = Nothing
instance GeoConvertable [GeoField] where
  cvtGeo (DataArray arr) = Just arr
  cvtGeo _ = Nothing
instance GeoConvertable T.Text where
  cvtGeo (DataString txt) = Just txt
  cvtGeo _ = Nothing
instance GeoConvertable Double where
  cvtGeo (DataDouble d) = Just d
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

-- | Parse number of given length
parseNumber :: Num a => Int64 -> Get a
parseNumber fsize = do
  bytes <- getBytes (fromIntegral fsize)
  return $ BS.foldl' (\acc new -> fromIntegral new + 256 * acc) 0 bytes

instance Serialize GeoField where
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
        2 -> DataString . decodeUtf8 <$> getBytes (fromIntegral fsize)
        3 -> DataDouble . wordToDouble <$> get
        5 -> DataWord <$> parseNumber fsize
        6 -> DataWord <$> parseNumber fsize
        7 -> do
            pairs <- replicateM (fromIntegral fsize) $ do
                    key <- get
                    val <- get
                    return (key, val)
            return $ DataMap (Map.fromList pairs)
        8 -> DataWord <$> parseNumber fsize
        9 -> DataWord <$> parseNumber fsize
        11 -> DataArray <$> replicateM (fromIntegral fsize) get
        14 -> return $ DataBool (fsize == 0)
        _ -> do
          _ <- getBytes (fromIntegral fsize)
          return $ DataUnknown ftype fsize
