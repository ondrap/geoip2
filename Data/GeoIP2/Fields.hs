{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ExistentialQuantification     #-}
{-# LANGUAGE RankNTypes     #-}

module Data.GeoIP2.Fields where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative  ((<$>))
#endif

import           Control.Monad        (replicateM)
import           Data.Serialize
import           Data.Reflection      (Given, given)
import           Data.Bits            (shift, (.&.))
import qualified Data.ByteString      as BS
import           Data.Int
import qualified Data.Map             as Map
import           Data.ReinterpretCast (wordToDouble)
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8)
import           Data.Word
import           Data.Void
import           Control.Lens.TH      (makePrisms)

data GeoFieldT a =
    DataPointer a
  | DataString !T.Text
  | DataDouble Double
  | DataInt Int64
  | DataWord Word64
  | DataMap (Map.Map (GeoFieldT a) (GeoFieldT a))
  | DataArray [GeoFieldT a]
  | DataBool Bool
  | DataUnknown Word8 Int64
  deriving (Eq, Ord)
-- Field with pointers resolved
type GeoField = GeoFieldT Void
deriving instance Show GeoField
-- Raw field with pointers
type GeoFieldRaw = GeoFieldT Int64

makePrisms ''GeoFieldT

-- | Go through the pointers and try to resolve them; we won't define instance of Applicative given that the 'key' to the Map is parametrized
traversePtr :: (Ord a, Applicative m) => (Int64 -> m (GeoFieldT a)) -> GeoFieldRaw -> m (GeoFieldT a)
traversePtr _ (DataString t) = pure (DataString t)
traversePtr _ (DataDouble t) = pure (DataDouble t)
traversePtr _ (DataInt t) = pure (DataInt t)
traversePtr _ (DataWord t) = pure (DataWord t)
traversePtr _ (DataBool t) = pure (DataBool t)
traversePtr _ (DataUnknown a b) = pure (DataUnknown a b)
-- For map we have to traverse over both keys and values...
traversePtr f (DataMap dmap) = DataMap . Map.fromList <$> traverse travBoth (Map.toList dmap)
  where
    travBoth (key, val) = (,) <$> traversePtr f key <*> traversePtr f val
traversePtr f (DataArray darr) = DataArray <$> traverse (traversePtr f) darr
traversePtr f (DataPointer a) = f a

-- | Parse number of given length
parseNumber :: Num a => Int64 -> Get a
parseNumber fsize = do
  bytes <- getBytes (fromIntegral fsize)
  return $ BS.foldl' (\acc new -> fromIntegral new + 256 * acc) 0 bytes

data ReadPointer = ReadPointer (forall a . Serialize (GeoFieldT a) => Int64 -> Either String (GeoFieldT a))

instance Given ReadPointer => Serialize GeoField where
  put = error "Serialization not implemented"
  get = do
    field <- get
    traversePtr (either fail pure . readPtr) field
    where ReadPointer readPtr = given


instance Serialize GeoFieldRaw where
  put = error "Serialization not implemented"
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
            | otherwise -> error "Shouldn't happen, limited to 5 bits"

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
        14 -> return $ DataBool (fsize == 1)
        _ -> do
          _ <- getBytes (fromIntegral fsize)
          return $ DataUnknown ftype fsize
