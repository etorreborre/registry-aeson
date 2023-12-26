{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-
  An Encoder is used to encode a specific data type into a Aeson Object
  This module provides several functions to create encoders and assemble them into a registry of encoders.
-}

module Data.Registry.Aeson.Encoder
  ( module Data.Registry.Aeson.Encoder,
    module Data.Registry.Aeson.TH.Encoder,
    module Data.Registry.Aeson.JsonTerm,
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
import Data.Registry.Aeson.JsonTerm
import Data.Registry.Aeson.JsonTerm as JsonTerm
import Data.Registry.Aeson.TH.Encoder
import Data.Registry.Aeson.TH.ThOptions
import Data.Set qualified as S
import Protolude as P hiding (Type)
import Prelude (String)

-- * ENCODER DATA TYPE

newtype Encoder a = Encoder {encode :: a -> JsonTerm}

instance Contravariant Encoder where
  contramap f (Encoder e) = Encoder (\a -> e (f a))

newtype KeyEncoder a = KeyEncoder {encodeAsKey :: a -> Key}

instance Contravariant KeyEncoder where
  contramap f (KeyEncoder a) = KeyEncoder (a . f)

-- * ENCODE VALUES

encodeByteString :: Encoder a -> a -> ByteString
encodeByteString (Encoder e) a = BL.toStrict (encodingToLazyByteString $ e a <%> encodingJsonAlgebra)

encodeValue :: Encoder a -> a -> Value
encodeValue (Encoder e) a = e a <%> valueJsonAlgebra

-- -- * CREATE KEY ENCODERS

-- -- | Make a key encoder from a function returning some text
encodeKey :: forall a. (Typeable a) => (a -> Text) -> Typed (KeyEncoder a)
encodeKey f = fun (keyEncoder f)

keyEncoder :: (a -> Text) -> KeyEncoder a
keyEncoder f = KeyEncoder $ K.fromString . toS . f

-- -- * CREATE ENCODERS

-- -- | Create an Encoder from a function returning a Value
fromValue :: (a -> Value) -> Encoder a
fromValue f = Encoder (\a -> toJson (f a))

-- -- | Create an encoder from a Aeson instance
jsonEncoder :: forall a. (ToJSON a, Typeable a) => Typed (Encoder a)
jsonEncoder = fun (jsonEncoderOf @a)

jsonEncoderOf :: (ToJSON a) => Encoder a
jsonEncoderOf = Encoder \a -> toJson a

-- -- * COMBINATORS

-- | Create an Encoder for a (Maybe a)
encodeMaybeOf :: forall a. (Typeable a) => Typed (Encoder a -> Encoder (Maybe a))
encodeMaybeOf = fun (maybeOfEncoder @a)

maybeOfEncoder :: Encoder a -> Encoder (Maybe a)
maybeOfEncoder (Encoder e) = Encoder $ \case
  Nothing -> JsonTerm.null
  Just a -> e a

-- | Create an Encoder for a pair (a, b)
encodePairOf :: forall a b. (Typeable a, Typeable b) => Typed (Encoder a -> Encoder b -> Encoder (a, b))
encodePairOf = fun (pairOfEncoder @a @b)

pairOfEncoder :: Encoder a -> Encoder b -> Encoder (a, b)
pairOfEncoder (Encoder ea) (Encoder eb) =
  Encoder $ \(a, b) -> JsonTerm.array [ea a, eb b]

-- | Create an Encoder for a tripe (a, b, c)
encodeTripleOf :: forall a b c. (Typeable a, Typeable b, Typeable c) => Typed (Encoder a -> Encoder b -> Encoder c -> Encoder (a, b, c))
encodeTripleOf = fun (tripleOfEncoder @a @b @c)

tripleOfEncoder :: Encoder a -> Encoder b -> Encoder c -> Encoder (a, b, c)
tripleOfEncoder (Encoder ea) (Encoder eb) (Encoder ec) =
  Encoder $ \(a, b, c) -> JsonTerm.array [ea a, eb b, ec c]

-- | Create an Encoder for a Set a
encodeSetOf :: forall a. (Typeable a) => Typed (Encoder a -> Encoder (Set a))
encodeSetOf = fun (setOfEncoder @a)

setOfEncoder :: Encoder a -> Encoder (Set a)
setOfEncoder (Encoder ea) = Encoder $ \as ->
  JsonTerm.array ((\a -> ea a) <$> S.toList as)

-- | Create an Encoder for a list [a]
encodeListOf :: forall a. (Typeable a) => Typed (Encoder a -> Encoder [a])
encodeListOf = fun (listOfEncoder @a)

listOfEncoder :: Encoder a -> Encoder [a]
listOfEncoder (Encoder ea) = Encoder $ \as ->
  JsonTerm.array ((\a -> ea a) <$> as)

-- | Create an Encoder for a map a b
encodeMapOf :: forall a b. (Typeable a, Typeable b) => Typed (KeyEncoder a -> Encoder b -> Encoder (Map a b))
encodeMapOf = fun (mapOfEncoder @a @b)

mapOfEncoder :: KeyEncoder a -> Encoder b -> Encoder (Map a b)
mapOfEncoder (KeyEncoder ea) (Encoder eb) = Encoder $ \ms ->
  JsonTerm.object $ (\(k, v) -> JsonTerm.pair (ea k) (eb v)) <$> M.assocs ms

-- | Create an Encoder for a non-empty list (NonEmpty a)
encodeNonEmptyOf :: forall a. (Typeable a) => Typed (Encoder a -> Encoder (NonEmpty a))
encodeNonEmptyOf = fun (nonEmptyOfEncoder @a)

nonEmptyOfEncoder :: Encoder a -> Encoder (NonEmpty a)
nonEmptyOfEncoder = contramap toList . listOfEncoder

-- * DEFAULT VALUES

defaultEncoderOptions :: Registry _ _
defaultEncoderOptions =
  fun defaultConstructorEncoder
    <: fun textKeyEncoder
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
defaultConstructorEncoder :: ConstructorEncoder
defaultConstructorEncoder = ConstructorEncoder makeEncoderFromConstructor

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
                then JsonTerm.array values
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
          vs -> JsonTerm.array vs
        else do
          case values of
            [v] | unwrapUnaryRecords options -> v
            _ -> valuesToObject fieldNames values
    TwoElemArray ->
      if P.null fieldNames
        then case values of
          [] -> JsonTerm.string (toS constructorTag)
          [v] -> JsonTerm.array [JsonTerm.string constructorTag, v]
          _ -> JsonTerm.array [JsonTerm.string constructorTag, JsonTerm.array values]
        else case values of
          [v]
            | unwrapUnaryRecords options ->
                JsonTerm.array [JsonTerm.string constructorTag, v]
          _ ->
            JsonTerm.array [JsonTerm.string constructorTag, valuesToObject fieldNames values]
    ObjectWithSingleField -> do
      if P.null fieldNames
        then case values of
          [] -> JsonTerm.string constructorTag
          [v] -> JsonTerm.object [JsonTerm.pair (K.fromText constructorTag) v]
          _ -> JsonTerm.object [JsonTerm.pair (K.fromText constructorTag) (JsonTerm.array values)]
        else case values of
          [v]
            | unwrapUnaryRecords options ->
                JsonTerm.object [JsonTerm.pair (K.fromText constructorTag) v]
          _ -> do
            JsonTerm.object [JsonTerm.pair (K.fromText constructorTag) (valuesToObject fieldNames values)]
    TaggedObject tagFieldName contentsFieldName ->
      if P.null values
        then JsonTerm.object [JsonTerm.pair (K.fromText $ toS tagFieldName) (JsonTerm.string constructorTag)]
        else
          if P.null fieldNames
            then case values of
              [v] ->
                JsonTerm.object
                  [ JsonTerm.pair (K.fromText $ toS tagFieldName) (JsonTerm.string constructorTag),
                    JsonTerm.pair (K.fromText $ toS contentsFieldName) v
                  ]
              _ ->
                JsonTerm.object
                  [ JsonTerm.pair (K.fromText $ toS tagFieldName) (JsonTerm.string constructorTag),
                    JsonTerm.pair (K.fromText $ toS contentsFieldName) (JsonTerm.array values)
                  ]
            else
              JsonTerm.object
                ( JsonTerm.pair (K.fromText $ toS tagFieldName) (JsonTerm.string constructorTag)
                    : (uncurry JsonTerm.pair <$> zip fieldNamesKeys values)
                )

-- | Apply Options to the constructor name + field names
--   and remove Nothing values if necessary
modifyFromConstructorWithOptions :: Options -> FromConstructor -> FromConstructor
modifyFromConstructorWithOptions options fc = do
  let (fn, fv) =
        if omitNothingFields options && length (fromConstructorFieldNames fc) == length (fromConstructorValues fc)
          then unzip $ filter (not . JsonTerm.isNull . snd) $ zip (fromConstructorFieldNames fc) (fromConstructorValues fc)
          else (fromConstructorFieldNames fc, fromConstructorValues fc)
  fc
    { fromConstructorName = toS . constructorTagModifier options . toS $ fromConstructorName fc,
      fromConstructorFieldNames = toS . fieldLabelModifier options . toS <$> fn,
      fromConstructorValues = fv
    }

-- | Create an Object from a list of field names and a list of Values
--   both as a Value and as an Encoding
valuesToObject :: [Text] -> [JsonTerm] -> JsonTerm
valuesToObject fieldNames values =
  JsonTerm.object $ ((\(n, v) -> JsonTerm.pair (K.fromText n) v)) <$> zip fieldNames values
