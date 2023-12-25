{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-
  An Encoder is used to encode a specific data type into a Aeson Object
  This module provides several functions to create encoders and assemble them into a registry of encoders.
-}

module Data.Registry.Aeson.Encoder1
  ( module Data.Registry.Aeson.Encoder1,
    module Data.Registry.Aeson.TH.Encoder1,
    module Data.Registry.Aeson.TH.ThOptions,
  )
where

import Data.Aeson
import Data.Aeson.Encoding.Internal
import Data.Aeson.Key qualified as K
import Data.ByteString.Lazy qualified as BL (toStrict)
import Data.Functor.Contravariant
import Data.Map qualified as M
import Data.Registry
import Data.Registry.Aeson.JsonTerm as JsonTerm
import Data.Registry.Aeson.TH.Encoder1
import Data.Registry.Aeson.TH.ThOptions
import Data.Set qualified as S
import Data.Vector qualified as V
import Protolude as P hiding (Type, list)
import Prelude (String)

-- * ENCODER DATA TYPE

newtype Encoder1 a = Encoder1 {encode1 :: a -> JsonTerm}

instance Contravariant Encoder1 where
  contramap f (Encoder1 e) = Encoder1 (\a -> e (f a))

newtype KeyEncoder a = KeyEncoder {encodeAsKey :: a -> Key}

instance Contravariant KeyEncoder where
  contramap f (KeyEncoder a) = KeyEncoder (a . f)

-- * ENCODE VALUES

encodeByteString :: Encoder1 a -> a -> ByteString
encodeByteString (Encoder1 e) a = BL.toStrict (encodingToLazyByteString $ e a encodingJsonAlgebra)

encodeValue :: Encoder1 a -> a -> Value
encodeValue (Encoder1 e) a = e a valueJsonAlgebra

-- -- * CREATE KEY ENCODERS

-- -- | Make a key encoder from a function returning some text
encodeKey :: forall a. (Typeable a) => (a -> Text) -> Typed (KeyEncoder a)
encodeKey f = fun (keyEncoder f)

keyEncoder :: (a -> Text) -> KeyEncoder a
keyEncoder f = KeyEncoder $ K.fromString . toS . f

-- -- * CREATE ENCODERS

-- -- | Create an Encoder from a function returning a Value
fromValue :: (a -> Value) -> Encoder1 a
fromValue f = Encoder1 (\a -> toJson (f a))

-- -- | Create an encoder from a Aeson instance
jsonEncoder :: forall a. (ToJSON a, Typeable a) => Typed (Encoder1 a)
jsonEncoder = fun (jsonEncoderOf @a)

jsonEncoderOf :: (ToJSON a) => Encoder1 a
jsonEncoderOf = Encoder1 \a -> toJson a

-- -- * COMBINATORS

-- | Create an Encoder for a (Maybe a)
encodeMaybeOf :: forall a. (Typeable a) => Typed (Encoder1 a -> Encoder1 (Maybe a))
encodeMaybeOf = fun (maybeOfEncoder @a)

maybeOfEncoder :: Encoder1 a -> Encoder1 (Maybe a)
maybeOfEncoder (Encoder1 e) = Encoder1 $ \case
  Nothing -> JsonTerm.null
  Just a -> e a

-- | Create an Encoder for a pair (a, b)
encodePairOf :: forall a b. (Typeable a, Typeable b) => Typed (Encoder1 a -> Encoder1 b -> Encoder1 (a, b))
encodePairOf = fun (pairOfEncoder @a @b)

pairOfEncoder :: Encoder1 a -> Encoder1 b -> Encoder1 (a, b)
pairOfEncoder (Encoder1 ea) (Encoder1 eb) =
  Encoder1 $ \(a, b) -> JsonTerm.list [ea a, eb b]

-- | Create an Encoder for a tripe (a, b, c)
encodeTripleOf :: forall a b c. (Typeable a, Typeable b, Typeable c) => Typed (Encoder1 a -> Encoder1 b -> Encoder1 c -> Encoder1 (a, b, c))
encodeTripleOf = fun (tripleOfEncoder @a @b @c)

tripleOfEncoder :: Encoder1 a -> Encoder1 b -> Encoder1 c -> Encoder1 (a, b, c)
tripleOfEncoder (Encoder1 ea) (Encoder1 eb) (Encoder1 ec) =
  Encoder1 $ \(a, b, c) -> JsonTerm.list [ea a, eb b, ec c]

-- | Create an Encoder for a Set a
encodeSetOf :: forall a. (Typeable a) => Typed (Encoder1 a -> Encoder1 (Set a))
encodeSetOf = fun (setOfEncoder @a)

setOfEncoder :: Encoder1 a -> Encoder1 (Set a)
setOfEncoder (Encoder1 ea) = Encoder1 $ \as ->
  JsonTerm.list ((\a -> ea a) <$> S.toList as)

-- | Create an Encoder for a list [a]
encodeListOf :: forall a. (Typeable a) => Typed (Encoder1 a -> Encoder1 [a])
encodeListOf = fun (listOfEncoder @a)

listOfEncoder :: Encoder1 a -> Encoder1 [a]
listOfEncoder (Encoder1 ea) = Encoder1 $ \as ->
  JsonTerm.list ((\a -> ea a) <$> as)

-- | Create an Encoder for a map a b
encodeMapOf :: forall a b. (Typeable a, Typeable b) => Typed (KeyEncoder a -> Encoder1 b -> Encoder1 (Map a b))
encodeMapOf = fun (mapOfEncoder @a @b)

mapOfEncoder :: KeyEncoder a -> Encoder1 b -> Encoder1 (Map a b)
mapOfEncoder (KeyEncoder ea) (Encoder1 eb) = Encoder1 $ \ms ->
  JsonTerm.pairs $ (\(k, v) -> JsonTerm.pair (ea k) (eb v)) <$> M.assocs ms

-- | Create an Encoder for a non-empty list (NonEmpty a)
encodeNonEmptyOf :: forall a. (Typeable a) => Typed (Encoder1 a -> Encoder1 (NonEmpty a))
encodeNonEmptyOf = fun (nonEmptyOfEncoder @a)

nonEmptyOfEncoder :: Encoder1 a -> Encoder1 (NonEmpty a)
nonEmptyOfEncoder = contramap toList . listOfEncoder

-- | Shortcut function to create arrays
array :: [Value] -> Value
array = Array . V.fromList

-- * DEFAULT VALUES

defaultEncoderOptions :: Registry _ _
defaultEncoderOptions =
  -- fun defaultConstructorEncoder
  -- <:
  fun textKeyEncoder
    <: fun stringKeyEncoder
    <: val defaultOptions

textKeyEncoder :: KeyEncoder Text
textKeyEncoder = KeyEncoder K.fromText

stringKeyEncoder :: KeyEncoder String
stringKeyEncoder = KeyEncoder K.fromString

-- * BUILDING ENCODERS

-- | A ConstructorEncoder uses configuration options + type information extracted from
--   a given data type (with TemplateHaskell) in order to produce a JsonTerm
newtype ConstructorEncoder = ConstructorEncoder
  { encodeConstructor :: Options -> FromConstructor -> JsonTerm
  }

-- | Default implementation, it can be overridden in a registry
-- defaultConstructorEncoder :: ConstructorEncoder
-- defaultConstructorEncoder = ConstructorEncoder makeEncoderFromConstructor

-- | Minimum set of data extracted from a given type with Template Haskell
--   in order to create the appropriate encoder given an Options value
data FromConstructor = FromConstructor
  { -- | names of all the constructors of the type
    fromConstructorNames :: [Text],
    -- | types of all the constructors of the type
    fromConstructorTypes :: [Text],
    -- | name of the constructor for the value to encode
    fromConstructorName :: Text,
    -- | name of all the constructor fields
    fromConstructorFieldNames :: [Text],
    -- | encoded values of all the constructor fields
    fromConstructorValues :: [JsonTerm]
  }

-- | Make an Encoder from Options and the representation of a constructor for a given value to encode
makeEncoderFromConstructor :: Options -> FromConstructor -> JsonTerm
makeEncoderFromConstructor options fromConstructor = do
  let fc = modifyFromConstructorWithOptions options fromConstructor
  case fc of
    -- nullary constructors
    FromConstructor _ [] name _ _ ->
      if allNullaryToStringTag options
        then JsonTerm.string name
        else makeSumEncoding options fc
    -- single constructor
    FromConstructor [_] _ _ names values ->
      if tagSingleConstructors options
        then case values of
          [v] | sumEncoding options == UntaggedValue && unwrapUnaryRecords options -> v
          _ -> makeSumEncoding options fc
        else do
          case values of
            [v] ->
              if unwrapUnaryRecords options || P.null names
                then v
                else valuesToObject names values
            _ ->
              if P.null names
                then JsonTerm.list values
                else valuesToObject names values
    -- sum constructor
    _ ->
      makeSumEncoding options fc

makeSumEncoding :: Options -> FromConstructor -> JsonTerm
makeSumEncoding options (FromConstructor _constructorNames _constructorTypes constructorTag fieldNames values) = do
  let fieldNamesKeys = K.fromText <$> fieldNames
  case sumEncoding options of
    UntaggedValue ->
      if P.null fieldNames
        then case values of
          [] -> JsonTerm.string constructorTag
          [v] -> v
          vs -> JsonTerm.list vs
        else do
          case values of
            [v] | unwrapUnaryRecords options -> v
            _ -> valuesToObject fieldNames values
    TwoElemArray ->
      if P.null fieldNames
        then case values of
          [] -> JsonTerm.string (toS constructorTag)
          [v] -> JsonTerm.list [JsonTerm.string constructorTag, v]
          _ -> JsonTerm.list [JsonTerm.string constructorTag, JsonTerm.list values]
        else case values of
          [v]
            | unwrapUnaryRecords options ->
                JsonTerm.list [JsonTerm.string constructorTag, v]
          _ ->
            JsonTerm.list [JsonTerm.string constructorTag, valuesToObject fieldNames values]
    ObjectWithSingleField -> do
      if P.null fieldNames
        then case values of
          [] -> JsonTerm.string constructorTag
          [v] -> JsonTerm.pairs [JsonTerm.pair (K.fromText constructorTag) v]
          _ -> JsonTerm.pairs [JsonTerm.pair (K.fromText constructorTag) (JsonTerm.list values)]
        else case values of
          [v]
            | unwrapUnaryRecords options ->
                JsonTerm.pairs [JsonTerm.pair (K.fromText constructorTag) v]
          _ -> do
            JsonTerm.pairs [JsonTerm.pair (K.fromText constructorTag) (valuesToObject fieldNames values)]
    TaggedObject tagFieldName contentsFieldName ->
      if P.null values
        then JsonTerm.pairs [JsonTerm.pair (K.fromText $ toS tagFieldName) (JsonTerm.string constructorTag)]
        else
          if P.null fieldNames
            then case values of
              [v] ->
                JsonTerm.pairs
                  [ JsonTerm.pair (K.fromText $ toS tagFieldName) (JsonTerm.string constructorTag),
                    JsonTerm.pair (K.fromText $ toS contentsFieldName) v
                  ]
              _ ->
                JsonTerm.pairs
                  [ JsonTerm.pair (K.fromText $ toS tagFieldName) (JsonTerm.string constructorTag),
                    JsonTerm.pair (K.fromText $ toS contentsFieldName) (JsonTerm.list values)
                  ]
            else
              JsonTerm.pairs
                ( JsonTerm.pair (K.fromText $ toS tagFieldName) (JsonTerm.string constructorTag)
                    : (uncurry JsonTerm.pair <$> zip fieldNamesKeys values)
                )

-- | Apply Options to the constructor name + field names
--   and remove Nothing values if necessary
modifyFromConstructorWithOptions :: Options -> FromConstructor -> FromConstructor
modifyFromConstructorWithOptions options fc = do
  if omitNothingFields options && length (fromConstructorFieldNames fc) == length (fromConstructorValues fc)
    then do
      let (fn, fv) = unzip (filter (not . JsonTerm.isNull . snd) (zip (fromConstructorFieldNames fc) (fromConstructorValues fc)))
      fc
        { fromConstructorName = toS . constructorTagModifier options . toS $ fromConstructorName fc,
          fromConstructorFieldNames = toS . fieldLabelModifier options . toS <$> fn,
          fromConstructorValues = fv
        }
    else fc

-- | Create an Object from a list of field names and a list of Values
--   both as a Value and as an Encoding
valuesToObject :: [Text] -> [JsonTerm] -> JsonTerm
valuesToObject fieldNames values =
  JsonTerm.pairs $ ((\(n, v) -> JsonTerm.pair (K.fromText n) v)) <$> zip fieldNames values
