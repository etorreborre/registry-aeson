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

-- | This data type defines operations which can be used to create JSON terms
--   The exact representation is fixed by the type variable `a`.
--   In practice we use 2 representations: Aeson Values (in memory JSON values)
--   and Aeson Encodings (lazy bytestring)
data JsonAlgebra a = JsonAlgebra
  { string_ :: Text -> a,
    number_ :: Scientific -> a,
    bool_ :: Bool -> a,
    null_ :: a,
    pair_ :: A.Key -> a -> Pair a,
    object_ :: [Pair a] -> a,
    array_ :: [a] -> a,
    toJson_ :: (Value, Encoding) -> a
  }

-- | Auxiliary data type to create a pair inside an object
data Pair a = Pair {pairKey :: A.Key, pairValue :: a}
  deriving (Eq, Show)

-- | Return the first element of the pair
firstPair :: Pair (a, b) -> Pair a
firstPair (Pair k (a, _)) = Pair k a

-- | Return the second element of the pair
secondPair :: Pair (a, b) -> Pair b
secondPair (Pair k (_, b)) = Pair k b

-- | Implementation of a JsonAlgebra returning a Value
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

    object_ :: [Pair Value] -> Value
    object_ = A.Object . KM.fromList . fmap (\(Pair k v) -> (k, v))

    array_ :: [Value] -> Value
    array_ = A.Array . V.fromList

    toJson_ :: (Value, Encoding) -> Value
    toJson_ = fst

-- | Implementation of a JsonAlgebra returning an Encoding
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

    object_ :: [Pair Encoding] -> Encoding
    object_ = E.pairs . foldMap identity . fmap (\(Pair k v) -> E.pair k v)

    array_ :: [Encoding] -> Encoding
    array_ = E.list identity

    toJson_ :: (Value, Encoding) -> Encoding
    toJson_ = snd

-- | Wrapper data type for a polymorphic JSON term
--   (polymorphic in the sense that it can be represented by either a Value or an Encoding)
--   It is necessary to use a newtype instead of a type alias in order to get around some limitations
--   with type inference. Note also that the ImpredicativeTypes language extension is required
newtype JsonTerm = JsonTerm {term :: forall a. JsonAlgebra a -> a}

-- | Apply a specific algebra implementation to a term
(<%>) :: JsonTerm -> JsonAlgebra a -> a
(<%>) (JsonTerm t) j = t j

-- * BUILDING FUNCTIONS

-- | Create a String term
string :: Text -> JsonTerm
string t = JsonTerm $ \ja -> string_ ja t

-- | Create a number term from an Int
int :: Int -> JsonTerm
int n = scientific (fromInteger . integerFromInt $ n)

-- | Create a number term from an Integer
integer :: Integer -> JsonTerm
integer n = scientific (fromInteger n)

-- | Create a number term from a Double
double :: Double -> JsonTerm
double n = scientific (fromFloatDigits n)

-- | Create a number term from a Float
float :: Float -> JsonTerm
float n = scientific (fromFloatDigits n)

-- | Create a number term from a Scientific
scientific :: Scientific -> JsonTerm
scientific n = JsonTerm $ \ja -> number_ ja n

-- | Create a boolean term
bool :: Bool -> JsonTerm
bool b = JsonTerm $ \ja -> bool_ ja b

-- | Create a Null term
null :: JsonTerm
null = JsonTerm $ \ja -> null_ ja

-- | Create a pair term, from key and another term
pair :: A.Key -> JsonTerm -> (forall a. JsonAlgebra a -> Pair a)
pair k v ja = pair_ ja k (v <%> ja)

-- | Create an object from a list of pairs
object :: [(forall a. JsonAlgebra a -> Pair a)] -> JsonTerm
object vs = JsonTerm $ \ja -> object_ ja ((\v -> v ja) <$> vs)

-- | Create an object from a single pair
single :: A.Key -> JsonTerm -> JsonTerm
single k v = object [pair k v]

-- | Create an array from a list of terms
array :: [JsonTerm] -> JsonTerm
array vs = JsonTerm $ \ja -> array_ ja ((\a -> a <%> ja) <$> vs)

-- | Create a term from any type having a ToJSON instance
toJson :: (ToJSON a) => a -> JsonTerm
toJson a = JsonTerm $ \j -> toJson_ j (A.toJSON a, A.toEncoding a)

-- | Return True if the term can be represented by the Null value
isNull :: JsonTerm -> Bool
isNull j = j <%> valueJsonAlgebra == A.Null
