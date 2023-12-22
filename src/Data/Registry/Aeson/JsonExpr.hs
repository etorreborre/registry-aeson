{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.Registry.Aeson.JsonExpr where

import Data.Aeson (Value, ToJSON)
import Data.Aeson qualified as A
import Data.Aeson.Encoding (Encoding)
import Data.Aeson.Encoding qualified as E
import Data.Aeson.KeyMap qualified as KM
import Data.Registry.Aeson.Encoder (Encoder (..), defaultEncoderOptions)
import Data.Registry
import Data.Scientific hiding (scientific)
import Data.Vector qualified as V
import GHC.Num.Integer
import Protolude hiding (bool, list, null)

data JsonExpr a = JsonExpr
  { string_ :: Text -> a,
    number_ :: Scientific -> a,
    bool_ :: Bool -> a,
    null_ :: a,
    pair_ :: A.Key -> a -> Pair a,
    pairs_ :: [Pair a] -> a,
    list_ :: [a] -> a,
    toJson_ :: (Value, Encoding) -> a
  }

-- pairJsonExpr :: JsonExpr a -> JsonExpr b -> JsonExpr (a, b)
-- pairJsonExpr ja jb =
--   JsonExpr
--     { string_ = \t -> (string_ ja t, string_ jb t),
--       number_ = \n -> (number_ ja n, number_ jb n),
--       bool_ = \b -> (bool_ ja b, bool_ jb b),
--       null_ = (null_ ja, null_ jb),
--       pair_ = Pair,
--       pairs_ = \vs -> (pairs_ ja (firstPair <$> vs), pairs_ jb (secondPair <$> vs)),
--       list_ = \vs -> (list_ ja (fst <$> vs), list_ jb (snd <$> vs))
--     }

data Pair a = Pair {pairKey :: A.Key, pairValue :: a}
  deriving (Eq, Show)

firstPair :: Pair (a, b) -> Pair a
firstPair (Pair k (a, _)) = Pair k a

secondPair :: Pair (a, b) -> Pair b
secondPair (Pair k (_, b)) = Pair k b


valueJsonExpr :: JsonExpr Value
valueJsonExpr = JsonExpr {..}
  where
    string_ :: Text -> Value
    string_ = A.String

    number_ :: Scientific -> Value
    number_ = A.Number

    bool_ :: Bool -> Value
    bool_ = A.Bool

    null_ :: Value
    null_ = A.Null

    pair_ :: A.Key -> Value -> Pair Value
    pair_ = Pair

    pairs_ :: [Pair Value] -> Value
    pairs_ = A.Object . KM.fromList . fmap (\(Pair k v) -> (k, v))

    list_ :: [Value] -> Value
    list_ = A.Array . V.fromList

    toJson_ :: (Value, Encoding) -> Value
    toJson_ = fst

encodingJsonExpr :: JsonExpr Encoding
encodingJsonExpr = JsonExpr {..}
  where
    string_ :: Text -> Encoding
    string_ = E.text

    number_ :: Scientific -> Encoding
    number_ = E.scientific

    bool_ :: Bool -> Encoding
    bool_ = E.bool

    null_ :: Encoding
    null_ = E.null_

    pair_ :: A.Key -> Encoding -> Pair Encoding
    pair_ = Pair

    pairs_ :: [Pair Encoding] -> Encoding
    pairs_ = E.pairs . foldMap identity . fmap (\(Pair k v) -> E.pair k v)

    list_ :: [Encoding] -> Encoding
    list_ = E.list identity

    toJson_ :: (Value, Encoding) -> Encoding
    toJson_ = snd


string :: Text -> JsonExpr a -> a
string = flip string_

int :: Int -> JsonExpr a -> a
int = scientific . fromInteger . integerFromInt

integer :: Integer -> JsonExpr a -> a
integer = scientific . fromInteger

double :: Double -> JsonExpr a -> a
double = scientific . fromFloatDigits

float :: Float -> JsonExpr a -> a
float = scientific . fromFloatDigits

scientific :: Scientific -> JsonExpr a -> a
scientific = flip number_

bool :: Bool -> JsonExpr a -> a
bool = flip bool_

null :: JsonExpr a -> a
null = null_

pair :: A.Key -> a -> JsonExpr a -> Pair a
pair k v ja = pair_ ja k v

pairs :: [JsonExpr a -> Pair a] -> JsonExpr a -> a
pairs vs ja = pairs_ ja (($ ja) <$> vs)

list :: [JsonExpr a -> a] -> JsonExpr a -> a
list vs ja = list_ ja (($ ja) <$> vs)

toJson :: ToJSON a => a -> JsonExpr b -> b
toJson a j = toJson_ j (A.toJSON a, A.toEncoding a)

both :: (forall a. JsonExpr a -> a) -> (Value, Encoding)
both e = (e valueJsonExpr, e encodingJsonExpr)

example :: Encoder1 Int -> Encoder1 (Int, Bool)
example ei = Encoder1 \(i, b) -> do
  list [encode1 ei i, bool b]


newtype Encoder1 a = Encoder1 {encode1 :: a -> (forall b. JsonExpr b -> b)}

encodeValue1 :: Encoder1 Int -> Value
encodeValue1 ei = (encode1 $ example ei) (1, True) valueJsonExpr

jsonEncoder1 :: forall a. (ToJSON a, Typeable a) => Typed (Encoder1 a)
jsonEncoder1 = fun (jsonEncoder1Of @a)

jsonEncoder1Of :: ToJSON a => Encoder1 a
jsonEncoder1Of = Encoder1 \a -> toJson a

enc :: Registry _ _
enc =
       fun encodeValue1
    <: jsonEncoder1 @Int
    <: defaultEncoderOptions
