{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-
  An Encoder is used to encode a specific data type into a Aeson Object
  This module provides several functions to create encoders and assemble them into a registry of encoders.
-}

module Data.Registry.Aeson.Encoder where

import Control.Monad.Fail
import Data.Aeson
import Data.Aeson.Encoding.Internal
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL (toStrict)
import Data.Functor.Contravariant
import Data.List (nub)
import Data.Registry
import Data.Registry.Aeson.TH
import Data.Registry.Internal.Types hiding (Value)
import qualified Data.Vector as V
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude hiding (Type, list)

-- * ENCODER DATA TYPE

newtype Encoder a = Encoder {encode :: a -> (Value, Encoding)}

instance Contravariant Encoder where
  contramap f (Encoder a) = Encoder (a . f)

-- * ENCODE VALUES

encodeByteString :: Encoder a -> a -> ByteString
encodeByteString (Encoder e) = BL.toStrict . encodingToLazyByteString . snd . e

encodeValue :: Encoder a -> a -> Value
encodeValue (Encoder e) = fst . e

-- * CREATE ENCODERS

-- | Create an Encoder from a function returning a Value
fromValue :: (a -> Value) -> Encoder a
fromValue f = Encoder $ \a -> let v = f a in (v, value v)

-- | Create an encoder from a Aeson instance
jsonEncoder :: forall a. (ToJSON a, Typeable a) => Typed (Encoder a)
jsonEncoder = fun (jsonEncoderOf @a)

jsonEncoderOf :: ToJSON a => Encoder a
jsonEncoderOf = Encoder $ \a -> (toJSON a, toEncoding a)

-- * COMBINATORS

-- | Create an Encoder for a (Maybe a)
encodeMaybeOf :: forall a. (Typeable a) => Typed (Encoder a -> Encoder (Maybe a))
encodeMaybeOf = fun (maybeOfEncoder @a)

maybeOfEncoder :: Encoder a -> Encoder (Maybe a)
maybeOfEncoder (Encoder e) = Encoder $ \case
  Nothing -> (Null, null_)
  Just a -> e a

-- | Create an Encoder for a pair (a, b)
encodePairOf :: forall a b. (Typeable a, Typeable b) => Typed (Encoder a -> Encoder b -> Encoder (a, b))
encodePairOf = fun (pairOfEncoder @a @b)

pairOfEncoder :: Encoder a -> Encoder b -> Encoder (a, b)
pairOfEncoder (Encoder ea) (Encoder eb) =
  Encoder $ \(a, b) -> do
    let (ls1, ls2) = unzip [ea a, eb b]
    (array ls1, list identity ls2)

-- | Create an Encoder for a tripe (a, b, c)
encodeTripleOf :: forall a b c. (Typeable a, Typeable b, Typeable c) => Typed (Encoder a -> Encoder b -> Encoder c -> Encoder (a, b, c))
encodeTripleOf = fun (tripleOfEncoder @a @b @c)

tripleOfEncoder :: Encoder a -> Encoder b -> Encoder c -> Encoder (a, b, c)
tripleOfEncoder (Encoder ea) (Encoder eb) (Encoder ec) =
  Encoder $ \(a, b, c) -> do
    let (ls1, ls2) = unzip [ea a, eb b, ec c]
    (array ls1, list identity ls2)

-- | Create an Encoder for a list [a]
encodeListOf :: forall a. (Typeable a) => Typed (Encoder a -> Encoder [a])
encodeListOf = fun (listOfEncoder @a)

listOfEncoder :: Encoder a -> Encoder [a]
listOfEncoder (Encoder ea) = Encoder $ \as -> do
  let (ls1, ls2) = unzip (ea <$> as)
  (array ls1, list identity ls2)

-- | Create an Encoder for a non-empty list (NonEmpty a)
encodeNonEmptyOf :: forall a. (Typeable a) => Typed (Encoder a -> Encoder (NonEmpty a))
encodeNonEmptyOf = fun (nonEmptyOfEncoder @a)

nonEmptyOfEncoder :: Encoder a -> Encoder (NonEmpty a)
nonEmptyOfEncoder = contramap toList . listOfEncoder

-- | Shortcut function to create arrays
array :: [Value] -> Value
array = Array . V.fromList

-- * TEMPLATE HASKELL

-- | Make an Encoder for a given data type
--   Usage: $(makeEncoder ''MyDataType) <: otherEncoders
makeEncoder :: Name -> ExpQ
makeEncoder encodedType = appE (varE $ mkName "fun") $ do
  info <- reify encodedType
  case info of
    TyConI (NewtypeD _context _name _typeVars _kind constructor _deriving) ->
      makeConstructorsEncoder [constructor]
    TyConI (DataD _context _name _typeVars _kind constructors _deriving) ->
      makeConstructorsEncoder constructors
    other -> do
      qReport True ("can only create encoders for an ADT, got: " <> show other)
      fail "encoders creation failed"

-- \(o::Options) (e0::Encoder A0) (e1::Encoder A1) ... -> Encoder $ \a ->
--   case a of
--     T1 a0 a1 ... -> makeEncoderFromConstructor o (FromConstructor names types "T1" fieldNames [encode e0 a0, encode e1 a1, ...])
--     T2 a0 a4 ... -> makeEncoderFromConstructor o (FromConstructor names types "T2" fieldNames [encode e0 a0, encode e4 a4, ...])
makeConstructorsEncoder :: [Con] -> ExpQ
makeConstructorsEncoder cs = do
  -- get the types of all the fields of all the constructors
  ts <- nub . join <$> for cs typesOf
  constructorsNames <- for cs nameOf
  let options = sigP (varP (mkName "os")) (conT $ mkName "Options")
  let encoderParameters = options : ((\(t, n) -> sigP (varP (mkName $ "e" <> show n)) (appT (conT $ mkName "Encoder") (pure t))) <$> zip ts [0 ..])
  matchClauses <- for cs $ makeMatchClause constructorsNames ts
  lamE encoderParameters (appE (conE (mkName "Encoder")) (lamCaseE (pure <$> matchClauses)))

-- | Make the match clause for a constructor given
--    - the list of all the encoder types
--    - the constructor name
--    - the constructor index in the list of all the constructors for the encoded data type
--   T1 a0 a1 ... -> makeEncoderFromConstructor o (FromConstructor names types cName fieldNames values)
makeMatchClause :: [Name] -> [Type] -> Con -> MatchQ
makeMatchClause constructorNames allTypes c = do
  ts <- typesOf c
  constructorTypes <- indexConstructorTypes allTypes ts
  cName <- dropQualified <$> nameOf c
  let names = listE $ litE . StringL . show . dropQualified <$> constructorNames
  let types = listE $ litE . StringL . show <$> allTypes
  fields <- fieldsOf c
  let fieldNames = listE $ litE . StringL . show . dropQualified <$> fields
  let params = conP (mkName $ show cName) $ (\(_, n) -> varP (mkName $ "a" <> show n)) <$> constructorTypes
  let values = listE $ (\(_, n) -> appE (appE (varE $ mkName "encode") (varE (mkName $ "e" <> show n))) (varE (mkName $ "a" <> show n))) <$> constructorTypes
  let encoded =
        varE (mkName "makeEncoderFromConstructor")
          `appE` varE (mkName "os")
          `appE` (conE (mkName "FromConstructor") `appE` names `appE` types `appE` litE (StringL $ show cName) `appE` fieldNames `appE` values)
  match params (normalB encoded) []

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
    fromConstructorValues :: [(Value, Encoding)]
  }
  deriving (Eq, Show)

-- | Apply Options to the constructor name + field names
--   and remove Nothing values if necessary
modifyFromConstructorWithOptions :: Options -> FromConstructor -> FromConstructor
modifyFromConstructorWithOptions options fc = do
  let (fn, fv) =
        if omitNothingFields options && length (fromConstructorFieldNames fc) == length (fromConstructorValues fc)
          then unzip $ filter ((/= Null) . fst . snd) $ zip (fromConstructorFieldNames fc) (fromConstructorValues fc)
          else (fromConstructorFieldNames fc, fromConstructorValues fc)
  fc
    { fromConstructorName = toS . constructorTagModifier options . toS $ fromConstructorName fc,
      fromConstructorFieldNames = toS . fieldLabelModifier options . toS <$> fn,
      fromConstructorValues = fv
    }

-- | Make an Encoder from Options and the representation of a constructor for a given value to encode
makeEncoderFromConstructor :: Options -> FromConstructor -> (Value, Encoding)
makeEncoderFromConstructor options fromConstructor = do
  let fc = modifyFromConstructorWithOptions options fromConstructor
  case fc of
    -- nullary constructors
    FromConstructor _ [] name _ _ ->
      if allNullaryToStringTag options
        then (String name, string $ toS name)
        else makeSumEncoding options fc
    -- single constructor
    FromConstructor [_] _ _ names values ->
      if tagSingleConstructors options
        then case (names, values) of
          (_, [v]) | sumEncoding options == UntaggedValue && unwrapUnaryRecords options -> v
          _ -> makeSumEncoding options fc
        else do
          case values of
            [(v, e)] ->
              if unwrapUnaryRecords options || null names
                then (v, e)
                else valuesToObject names values
            _ ->
              if null names
                then case values of
                  [v] -> v
                  _ -> do
                    let (vs, es) = unzip values
                    (array vs, list identity es)
                else valuesToObject names values
    -- sum constructor
    _ ->
      makeSumEncoding options fc

makeSumEncoding :: Options -> FromConstructor -> (Value, Encoding)
makeSumEncoding options (FromConstructor _constructorNames _constructorTypes constructorTag fieldNames values) = do
  let fieldNamesKeys = K.fromText <$> fieldNames
  case sumEncoding options of
    UntaggedValue ->
      if null fieldNames
        then do
          let (vs, es) = unzip values
          case (vs, es) of
            ([], []) -> (String $ toS constructorTag, string $ toS constructorTag)
            ([v], [e]) -> (v, e)
            _ -> (array vs, list identity es)
        else do
          let (vs, es) = unzip values
          case (vs, es) of
            ([v], [e]) | unwrapUnaryRecords options -> (v, e)
            _ -> valuesToObject fieldNames values
    TwoElemArray ->
      if null fieldNames
        then do
          let (vs, es) = unzip values
          case (vs, es) of
            ([], []) -> (String constructorTag, string $ toS constructorTag)
            ([v], [e]) -> (array [String constructorTag, v], list identity [string $ toS constructorTag, e])
            _ -> (array [String constructorTag, array vs], list identity [string $ toS constructorTag, list identity es])
        else do
          let (vs, es) = unzip values
          case (vs, es) of
            ([v], [e])
              | unwrapUnaryRecords options ->
                (array [String constructorTag, v], list identity [string $ toS constructorTag, e])
            _ -> do
              let (vs', es') = valuesToObject fieldNames values
              (array [String constructorTag, vs'], list identity [string $ toS constructorTag, es'])
    ObjectWithSingleField -> do
      if null fieldNames
        then do
          let (vs, es) = unzip values
          case (vs, es) of
            ([], []) -> (String constructorTag, string $ toS constructorTag)
            ([v], [e]) -> (Object $ KM.singleton (K.fromText constructorTag) v, pairs (pair (K.fromText constructorTag) e))
            _ -> (Object $ KM.singleton (K.fromText constructorTag) (array vs), pairs (pair (K.fromText constructorTag) $ list identity es))
        else do
          let (vs, es) = unzip values
          case (vs, es) of
            ([v], [e])
              | unwrapUnaryRecords options ->
                (Object $ KM.singleton (K.fromText constructorTag) v, pairs (pair (K.fromText constructorTag) e))
            _ -> do
              let (vs', es') = valuesToObject fieldNames values
              (Object $ KM.singleton (K.fromText constructorTag) vs', pairs (pair (K.fromText constructorTag) es'))
    TaggedObject tagFieldName contentsFieldName ->
      if null values
        then
          ( Object $ KM.fromList [(K.fromText $ toS tagFieldName, String constructorTag)],
            pairs (pair (K.fromText $ toS tagFieldName) (string $ toS constructorTag))
          )
        else
          if null fieldNames
            then case unzip values of
              ([v], [e]) ->
                ( Object $ KM.fromList [(K.fromText $ toS tagFieldName, String constructorTag), (K.fromText $ toS contentsFieldName, v)],
                  pairs $ pair (K.fromText $ toS tagFieldName) (string $ toS constructorTag) <> pair (K.fromText $ toS contentsFieldName) e
                )
              (vs, es) ->
                ( Object $ KM.fromList [(K.fromText $ toS tagFieldName, String constructorTag), (K.fromText $ toS contentsFieldName, array vs)],
                  pairs $ pair (K.fromText $ toS tagFieldName) (string $ toS constructorTag) <> pair (K.fromText $ toS contentsFieldName) (list identity es)
                )
            else do
              let (vs, es) = unzip values
              ( Object . KM.fromList $ (K.fromText $ toS tagFieldName, String constructorTag) : zip fieldNamesKeys vs,
                pairs (foldMap identity $ pair (K.fromText $ toS tagFieldName) (string $ toS constructorTag) : (uncurry pair <$> zip fieldNamesKeys es))
                )

-- | Create an Object from a list of field names and a list of Values
--   both as a Value and as an Encoding
valuesToObject :: [Text] -> [(Value, Encoding)] -> (Value, Encoding)
valuesToObject fieldNames values = do
  let (vs, es) = unzip values
  let fieldNamesKeys = K.fromText <$> fieldNames
  (Object $ KM.fromList (zip fieldNamesKeys vs), pairs $ foldMap identity (uncurry pair <$> zip fieldNamesKeys es))
