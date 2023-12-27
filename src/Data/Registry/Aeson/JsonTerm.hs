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
import Protolude hiding (bool, list, null, empty)

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
    empty_ :: r A.Key,
    concatenate_ :: r A.Key -> r A.Key -> r A.Key,
    object_ :: r A.Key -> r (),
    array_ :: [r ()] -> r (),
    toJson_ :: (Value, Encoding) -> r ()
  }

-- | Implementation of a JsonAlgebra returning single or multiple values of type `a`
data Values k where
  SingleValue :: Value -> Values ()
  ManyValues :: [(A.Key, Value)] -> Values A.Key

toValue :: Values k -> Value
toValue (SingleValue v) = v
toValue (ManyValues vs) = A.Object . KM.fromList $ vs

valueJsonAlgebra :: JsonAlgebra Values
valueJsonAlgebra = JsonAlgebra {..}
  where
    string_ :: Text -> Values ()
    string_ = SingleValue . A.String

    number_ :: Scientific -> Values  ()
    number_ = SingleValue . A.Number

    bool_ :: Bool -> Values ()
    bool_ = SingleValue . A.Bool

    null_ :: Values ()
    null_ = SingleValue A.Null

    pair_ :: A.Key -> Values () -> Values A.Key
    pair_ k (SingleValue v) = ManyValues [(k, v)]

    empty_ :: Values A.Key
    empty_ = ManyValues []

    concatenate_ :: Values A.Key -> Values A.Key -> Values A.Key
    concatenate_ (ManyValues vs1) (ManyValues vs2) = ManyValues (vs1 <> vs2)

    object_ :: Values A.Key -> Values ()
    object_ (ManyValues vs) = SingleValue . A.Object $ KM.fromList vs

    array_ :: [Values ()] -> Values ()
    array_ = SingleValue . A.Array . fmap (\(SingleValue v) -> v) . V.fromList

    toJson_ :: (Value, Encoding) -> Values ()
    toJson_ = SingleValue . fst


-- | Implementation of a JsonAlgebra returning an Encoding
data Encoded k where
  Encoded :: Encoding -> Encoded ()
  CommaSeparated :: E.Series -> Encoded A.Key

toEncoding :: Encoded k -> Encoding
toEncoding (Encoded e) = e
toEncoding (CommaSeparated s) = E.pairs s

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
    pair_ k (Encoded e) = CommaSeparated $ E.pair k e

    empty_ :: Encoded A.Key
    empty_ = CommaSeparated mempty

    concatenate_ :: Encoded A.Key -> Encoded A.Key -> Encoded A.Key
    concatenate_ (CommaSeparated s1) (CommaSeparated s2) = CommaSeparated (s1 <> s2)

    object_ :: Encoded A.Key -> Encoded ()
    object_ (CommaSeparated s) = Encoded $ E.pairs s

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
(<%>) = interpret

-- | Apply a specific algebra implementation to a term
interpret :: JsonTerm () -> JsonAlgebra r -> r ()
interpret (JsonTerm t) j = t j

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

-- | Create an empty list of pairs
empty :: (forall r. JsonAlgebra r -> r A.Key)
empty = empty_

-- | Concatenate 2 pair terms
concatenate :: (forall r. JsonAlgebra r -> r A.Key) -> (forall r. JsonAlgebra r -> r A.Key) -> (forall r. JsonAlgebra r -> r A.Key)
concatenate v1 v2 ja = concatenate_ ja (v1 ja) (v2 ja)

(><) :: (forall r. JsonAlgebra r -> r A.Key) -> (forall r. JsonAlgebra r -> r A.Key) -> (forall r. JsonAlgebra r -> r A.Key)
(><) = concatenate

-- | Create an object from a list of pairs
object :: (forall r. JsonAlgebra r -> r A.Key) -> JsonTerm ()
object vs = JsonTerm $ \ja -> object_ ja (vs ja)

-- | Create an object from a single pair
single :: A.Key -> JsonTerm () -> JsonTerm ()
single k v = object (pair k v)

-- | Create an object from a single pair
fold :: [(A.Key, JsonTerm ())] -> JsonTerm ()
fold vs = JsonTerm $ \ja -> object_ ja $ foldr (\(k, v) r -> concatenate_ ja (pair_ ja k (v <%> ja)) r) (empty_ ja) vs

-- | Create an array from a list of terms
array :: [JsonTerm ()] -> JsonTerm ()
array vs = JsonTerm $ \ja -> array_ ja ((\a -> a <%> ja) <$> vs)

-- | Create a term from any type having a ToJSON instance
toJson :: (ToJSON a) => a -> JsonTerm ()
toJson a = JsonTerm $ \j -> toJson_ j (A.toJSON a, A.toEncoding a)

-- | Return True if the term can be represented by the Null value
isNull :: JsonTerm () -> Bool
isNull j = toValue (j <%> valueJsonAlgebra) == A.Null
