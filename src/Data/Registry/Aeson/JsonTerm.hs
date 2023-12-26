{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Registry.Aeson.JsonTerm where

import Data.Aeson (ToJSON, Value)
import Data.Aeson qualified as A
import Data.Aeson.Encoding (Encoding)
import Data.Aeson.Encoding qualified as E
import Data.Aeson.KeyMap qualified as KM
import Data.Scientific hiding (scientific)
import Data.Vector qualified as V
import GHC.Num.Integer
import Protolude hiding (bool, list, null)

data JsonAlgebra a = JsonAlgebra
  { string_ :: Text -> a,
    number_ :: Scientific -> a,
    bool_ :: Bool -> a,
    null_ :: a,
    pair_ :: A.Key -> a -> Pair a,
    pairs_ :: [Pair a] -> a,
    list_ :: [a] -> a,
    toJson_ :: (Value, Encoding) -> a
  }

data Pair a = Pair {pairKey :: A.Key, pairValue :: a}
  deriving (Eq, Show)

firstPair :: Pair (a, b) -> Pair a
firstPair (Pair k (a, _)) = Pair k a

secondPair :: Pair (a, b) -> Pair b
secondPair (Pair k (_, b)) = Pair k b

valueJsonAlgebra :: JsonAlgebra Value
valueJsonAlgebra = JsonAlgebra {..}
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

encodingJsonAlgebra :: JsonAlgebra Encoding
encodingJsonAlgebra = JsonAlgebra {..}
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

newtype JsonTerm = JsonTerm {term :: forall a. JsonAlgebra a -> a}

(<%>) :: JsonTerm -> JsonAlgebra a -> a
(<%>) (JsonTerm t) j = t j

string :: Text -> JsonTerm
string t = JsonTerm $ \ja -> string_ ja t

int :: Int -> JsonTerm
int n = scientific (fromInteger . integerFromInt $ n)

integer :: Integer -> JsonTerm
integer n = scientific (fromInteger n)

double :: Double -> JsonTerm
double n = scientific (fromFloatDigits n)

float :: Float -> JsonTerm
float n = scientific (fromFloatDigits n)

scientific :: Scientific -> JsonTerm
scientific n = JsonTerm $ \ja -> number_ ja n

bool :: Bool -> JsonTerm
bool b = JsonTerm $ \ja -> bool_ ja b

null :: JsonTerm
null = JsonTerm $ \ja -> null_ ja

pair :: A.Key -> JsonTerm -> (forall a. JsonAlgebra a -> Pair a)
pair k v ja = pair_ ja k (v <%> ja)

pairs :: [(forall a. JsonAlgebra a -> Pair a)] -> JsonTerm
pairs vs = JsonTerm $ \ja -> pairs_ ja ((\v -> v ja) <$> vs)

list :: [JsonTerm] -> JsonTerm
list vs = JsonTerm $ \ja -> list_ ja ((\a -> a <%> ja) <$> vs)

toJson :: (ToJSON a) => a -> JsonTerm
toJson a = JsonTerm $ \j -> toJson_ j (A.toJSON a, A.toEncoding a)

both :: JsonTerm -> (Value, Encoding)
both e = (e <%> valueJsonAlgebra, e <%> encodingJsonAlgebra)

isNull :: JsonTerm -> Bool
isNull j = j <%> valueJsonAlgebra == A.Null
