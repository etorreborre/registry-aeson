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
data JsonAlgebra r = JsonAlgebra
  { string_ :: Text -> r (),
    number_ :: Scientific -> r (),
    bool_ :: Bool -> r (),
    null_ :: r (),
    pair_ :: A.Key -> r () -> r A.Key,
    object_ :: [r A.Key] -> r (),
    array_ :: [r ()] -> r (),
    toJson_ :: (Value, Encoding) -> r ()
  }

-- | Implementation of a JsonAlgebra returning a Value
data Valued k where
  Valued :: Value -> Valued ()
  Paired :: A.Key -> Valued () -> Valued A.Key

toValue :: Valued k -> Value
toValue (Valued v) = v
toValue (Paired k v) = A.Object $ KM.fromList [(k, toValue v)]

valueJsonAlgebra :: JsonAlgebra Valued
valueJsonAlgebra = JsonAlgebra {..}
  where
    string_ :: Text -> Valued ()
    string_ = Valued . A.String

    number_ :: Scientific -> Valued ()
    number_ = Valued . A.Number

    bool_ :: Bool -> Valued ()
    bool_ = Valued . A.Bool

    null_ :: Valued ()
    null_ = Valued A.Null

    pair_ :: A.Key -> Valued () -> Valued A.Key
    pair_ = Paired

    object_ :: [Valued A.Key] -> Valued ()
    object_ = Valued . A.Object . KM.fromList . fmap (\(Paired k (Valued v)) -> (k, v))

    array_ :: [Valued ()] -> Valued ()
    array_ = Valued . A.Array . fmap (\(Valued v) -> v) . V.fromList

    toJson_ :: (Value, Encoding) -> Valued ()
    toJson_ = Valued . fst


-- | Implementation of a JsonAlgebra returning an Encoding
data Encoded k where
  Encoded :: Encoding -> Encoded ()
  Keyed :: A.Key -> Encoded () -> Encoded A.Key

toEncoding :: Encoded k -> Encoding
toEncoding (Encoded v) = v
toEncoding (Keyed k v) = E.pairs (E.pair k (toEncoding v))

encodingJsonAlgebra :: JsonAlgebra Encoded
encodingJsonAlgebra = JsonAlgebra {..}
  where
    string_ :: Text -> Encoded ()
    string_ = Encoded . E.text

    number_ :: Scientific -> Encoded ()
    number_ = Encoded . E.scientific

    bool_ :: Bool -> Encoded ()
    bool_ = Encoded . E.bool

    null_ :: Encoded ()
    null_ = Encoded E.null_

    pair_ :: A.Key -> Encoded () -> Encoded A.Key
    pair_ = Keyed

    object_ :: [Encoded A.Key] -> Encoded ()
    object_ = Encoded . E.pairs . foldMap identity . fmap (\(Keyed k (Encoded v)) -> E.pair k v)

    array_ :: [Encoded ()] -> Encoded ()
    array_ = Encoded . E.list identity .fmap toEncoding

    toJson_ :: (Value, Encoding) -> Encoded ()
    toJson_ = Encoded . snd

-- | Wrapper data type for a polymorphic JSON term
--   (polymorphic in the sense that it can be represented by either a Value or an Encoding)
--   It is necessary to use a newtype instead of a type alias in order to get around some limitations
--   with type inference. Note also that the ImpredicativeTypes language extension is required
newtype JsonTerm a = JsonTerm {runTerm :: forall r. JsonAlgebra r -> r a}

-- | Apply a specific algebra implementation to a term
(<%>) :: JsonTerm () -> JsonAlgebra r -> r ()
(<%>) (JsonTerm t) j = t j

-- | Apply a specific algebra implementation to a term
makeValue :: JsonTerm a -> Value
makeValue (JsonTerm t) = toValue $ t valueJsonAlgebra

-- | Apply a specific algebra implementation to a term
makeEncoding :: JsonTerm a -> Encoding
makeEncoding (JsonTerm t) = toEncoding $ t encodingJsonAlgebra

-- * BUILDING FUNCTIONS

-- | Create a String term
string :: Text -> JsonTerm ()
string t = JsonTerm $ \ja -> string_ ja t

-- | Create a number term from an Int
int :: Int -> JsonTerm ()
int n = scientific (fromInteger . integerFromInt $ n)

-- | Create a number term from an Integer
integer :: Integer -> JsonTerm ()
integer n = scientific (fromInteger n)

-- | Create a number term from a Double
double :: Double -> JsonTerm ()
double n = scientific (fromFloatDigits n)

-- | Create a number term from a Float
float :: Float -> JsonTerm ()
float n = scientific (fromFloatDigits n)

-- | Create a number term from a Scientific
scientific :: Scientific -> JsonTerm ()
scientific n = JsonTerm $ \ja -> number_ ja n

-- | Create a boolean term
bool :: Bool -> JsonTerm ()
bool b = JsonTerm $ \ja -> bool_ ja b

-- | Create a Null term
null :: JsonTerm ()
null = JsonTerm $ \ja -> null_ ja

-- | Create a pair term, from key and another term
pair :: A.Key -> JsonTerm () -> (forall r. JsonAlgebra r -> r A.Key)
pair k v ja = pair_ ja k (v <%> ja)

-- | Create an object from a list of pairs
object :: [(forall r. JsonAlgebra r -> r A.Key)] -> JsonTerm ()
object vs = JsonTerm $ \ja -> object_ ja ((\v -> v ja) <$> vs)

-- | Create an object from a single pair
single :: A.Key -> JsonTerm () -> JsonTerm ()
single k v = object [pair k v]

-- | Create an array from a list of terms
array :: [JsonTerm ()] -> JsonTerm ()
array vs = JsonTerm $ \ja -> array_ ja ((\a -> a <%> ja) <$> vs)

-- | Create a term from any type having a ToJSON instance
toJson :: (ToJSON a) => a -> JsonTerm ()
toJson a = JsonTerm $ \j -> toJson_ j (A.toJSON a, A.toEncoding a)

-- | Return True if the term can be represented by the Null value
isNull :: JsonTerm () -> Bool
isNull j = toValue (j <%> valueJsonAlgebra) == A.Null
