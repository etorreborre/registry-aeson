{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-
  A Decoder is used to decode a Aeson Object into a specific data type
  This module provides several functions to create decoders and assemble them into a registry of encoders.
-}
module Data.Registry.Aeson.Decoder where

import Control.Monad.Fail
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.List (nub, (\\))
import Data.Registry
import Data.Registry.Aeson.TH
import Data.Registry.Internal.Types hiding (Value)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vector
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude as P hiding (Type)
import Prelude (String, show)

-- * DECODER DATA TYPE

newtype Decoder a = Decoder {decodeValue :: Value -> Either Text a}

instance Functor Decoder where
  fmap f (Decoder d) = Decoder (fmap f . d)

instance Applicative Decoder where
  pure a = Decoder (const (pure a))
  f <*> a = uncurry ($) <$> decoderAp f a

decoderAp :: Decoder a -> Decoder b -> Decoder (a, b)
decoderAp (Decoder da) (Decoder db) = Decoder $ \case
  o@(Array ls) ->
    case reverse (toList ls) of
      b : as -> (,) <$> da (Array $ Vector.fromList $ reverse as) <*> db b
      [] -> (,) <$> da o <*> db o
  o -> (,) <$> da o <*> db o

-- * DECODING

-- | Use a Decoder to decode a ByteString into the desired type
decodeByteString :: forall a. (Typeable a) => Decoder a -> BL.ByteString -> Either Text a
decodeByteString d bs =
  case eitherDecode bs of
    Left e -> Left $ "Cannot parse the string as a Value: " <> P.show e <> ". The string is: " <> P.show bs
    Right v ->
      case decodeValue d v of
        Right a -> pure a
        Left e -> Left $ "Cannot decode the type '" <> toS (showType @a) <> "' >> " <> e

-- * CREATING DECODERS

-- | Add a Decoder a to a registry of decoders when a Aeson a instance exists
--   usage: decoders = jsonDecoder @a <: otherDecoders
jsonDecoder :: forall a. (FromJSON a, Typeable a) => Typed (Decoder a)
jsonDecoder = fun (jsonDecoderOf @a)

jsonDecoderOf :: FromJSON a => Decoder a
jsonDecoderOf = Decoder $ \v ->
  case fromJSON v of
    Success a -> Right a
    Error e -> Left (toS e)

-- * COMBINATORS

-- | Add a Maybe (Decoder a) to a registry of decoders
--   usage: decoders = decodeMaybeOf @a <: otherDecoders
--   the list of otherDecoders must contain a Decoder a
--   otherwise there will be a compilation error
decodeMaybeOf :: forall a. (Typeable a) => Typed (Decoder a -> Decoder (Maybe a))
decodeMaybeOf = fun (maybeOfDecoder @a)

maybeOfDecoder :: forall a. Decoder a -> Decoder (Maybe a)
maybeOfDecoder (Decoder d) = Decoder $ \case
  Null -> pure Nothing
  just -> Just <$> d just

-- | Add a Maybe (a, b) to a registry of decoders
--   usage: decoders = decodePairOf @a @b <: otherDecoders
--   the list of otherDecoders must contain a Decoder a and a Decoder b
--   otherwise there will be a compilation error
decodePairOf :: forall a b. (Typeable a, Typeable b) => Typed (Decoder a -> Decoder b -> Decoder (a, b))
decodePairOf = fun (pairOfDecoder @a @b)

pairOfDecoder :: forall a b. (Typeable a, Typeable b) => Decoder a -> Decoder b -> Decoder (a, b)
pairOfDecoder (Decoder a) (Decoder b) = Decoder $ \case
  Array [oa, ob] -> (,) <$> a oa <*> b ob
  _ -> Left . toS $ "not a pair of " <> showType @a <> "," <> showType @b

-- | Add a Maybe (a, b, c) to a registry of decoders
--   usage: decoders = decodeTripleOf @a @b @c <: otherDecoders
--   the list of otherDecoders must contain a Decoder a, a Decoder b and a Decoder c
--   otherwise there will be a compilation error
decodeTripleOf :: forall a b c. (Typeable a, Typeable b, Typeable c) => Typed (Decoder a -> Decoder b -> Decoder c -> Decoder (a, b, c))
decodeTripleOf = fun (tripleOfDecoder @a @b @c)

tripleOfDecoder :: forall a b c. (Typeable a, Typeable b, Typeable c) => Decoder a -> Decoder b -> Decoder c -> Decoder (a, b, c)
tripleOfDecoder (Decoder a) (Decoder b) (Decoder c) = Decoder $ \case
  Array [oa, ob, oc] -> (,,) <$> a oa <*> b ob <*> c oc
  _ -> Left . toS $ "not a triple of " <> showType @a <> "," <> showType @b <> "," <> showType @c

-- | Add a Decoder [a] to a registry of decoders
--   usage: decoders = decodeListOf @a <: otherDecoders
--   the list of otherDecoders must contain a Decoder a
--   otherwise there will be a compilation error
decodeListOf :: forall a. (Typeable a) => Typed (Decoder a -> Decoder [a])
decodeListOf = fun (listOfDecoder @a)

listOfDecoder :: forall a. (Typeable a) => Decoder a -> Decoder [a]
listOfDecoder (Decoder a) = Decoder $ \case
  Array vs -> for (toList vs) a
  _ -> Left . toS $ "not a list of " <> showType @a

-- | Add a Decoder (NonEmpty a) to a registry of decoders
--   usage: decoders = decodeNonEmptyOf @a <: otherDecoders
--   the list of otherDecoders must contain a Decoder a
--   otherwise there will be a compilation error
decodeNonEmptyOf :: forall a. (Typeable a) => Typed (Decoder a -> Decoder (NonEmpty a))
decodeNonEmptyOf = fun (nonEmptyOfDecoder @a)

nonEmptyOfDecoder :: forall a. (Typeable a) => Decoder a -> Decoder (NonEmpty a)
nonEmptyOfDecoder (Decoder a) = Decoder $ \case
  Array values ->
    case toList values of
      [] -> Left . toS $ "expected a NonEmpty of " <> showType @a
      o : os -> (:|) <$> a o <*> for os a
  _ -> Left . toS $ "not a list of " <> showType @a

showType :: forall a. (Typeable a) => String
showType = P.show (typeRep (Proxy :: Proxy a))

-- * TEMPLATE HASKELL

-- | Make a Decoder for a given data type
--   Usage: $(makeDecoder ''MyDataType <: otherDecoders)
makeDecoder :: Name -> ExpQ
makeDecoder typeName = appE (varE $ mkName "fun") $ do
  info <- reify typeName
  case info of
    TyConI (NewtypeD _context _name _typeVars _kind constructor _deriving) ->
      makeConstructorsDecoder typeName [constructor]
    TyConI (DataD _context _name _typeVars _kind constructors _deriving) -> do
      case constructors of
        [] -> do
          qReport True "can not make an Decoder for an empty data type"
          fail "decoders creation failed"
        _ -> makeConstructorsDecoder typeName constructors
    other -> do
      qReport True ("can only create decoders for an ADT, got: " <> P.show other)
      fail "decoders creation failed"

-- | Make a decoder for a given data type by extracting just enough metadata about the data type in order to be able
--   to parse a Value
--
--   For example for the data type:
--
--   data T = T1 {f1::Int, f2::Int} | T2 Int Int
--
--   we add this function to the registry:
--
--   \opts d1 d2 d3 -> Decoder $ \v ->
--     decodeFromDefinitions opts v $ \case
--       ToConstructor "T1" [v1, v2]-> T1 <$> d1 v1 <*> d2 v2 ...
--       ToConstructor "T2" [v1, v2]-> T2 <$> d1 v1 <*> d3 v2 ...
--       other -> Left ("cannot decode " <> valueToText v)
--
--   The \case function is the only one which needs to be generated in order to match the exact shape of the
--   constructors to instantiate
makeConstructorsDecoder :: Name -> [Con] -> ExpQ
makeConstructorsDecoder typeName cs = do
  ts <- nub . join <$> for cs typesOf
  let decoderParameters = sigP (varP (mkName "os")) (conT $ mkName "Options") : ((\(t, n) -> sigP (varP (mkName $ "d" <> P.show n)) (appT (conT $ mkName "Decoder") (pure t))) <$> zip ts [0 ..])
  -- makeToConstructors os [Constructor "T1" ["f1", "f2"], Constructor "T2" []] v
  let paramP = varP (mkName "v")
  constructorDefs <- for cs $ \c -> do
    cName <- dropQualified <$> nameOf c
    fields <- fmap (litE . StringL . P.show . dropQualified) <$> fieldsOf c
    fieldTypes <- fmap (litE . StringL . P.show . getSimpleTypeName) <$> typesOf c
    varE (mkName "makeConstructorDef") `appE` (litE . StringL $ P.show cName) `appE` listE fields `appE` listE fieldTypes
  let matchClauses = makeMatchClause typeName ts <$> cs
  let matchFunction = lamCaseE (matchClauses <> [makeErrorClause typeName])
  let resolveFunction = varE (mkName "decodeFromDefinitions") `appE` varE (mkName "os") `appE` listE (pure <$> constructorDefs) `appE` varE (mkName "v") `appE` matchFunction
  lamE decoderParameters (appE (conE (mkName "Decoder")) (lamE [paramP] resolveFunction))

-- | Decode the nth constructor of a data type
--    ToConstructor "T1" [v1, v2]-> T1 <$> d1 v1 <*> d2 v2 ...
makeMatchClause :: Name -> [Type] -> Con -> MatchQ
makeMatchClause typeName allTypes c = do
  ts <- typesOf c
  constructorTypes <- fmap snd <$> indexConstructorTypes allTypes ts
  cName <- dropQualified <$> nameOf c
  let fieldsP = listP $ (\i -> varP $ mkName ("v" <> P.show i)) <$> constructorTypes
  match
    (conP (mkName "ToConstructor") [litP (StringL . P.show $ cName), fieldsP])
    (normalB (applyDecoder typeName cName constructorTypes))
    []

-- | Return an error the json value cannot be decoded with a constructor name and some values
makeErrorClause :: Name -> MatchQ
makeErrorClause typeName = do
  let errorMessage =
        (varE (mkName "<>") `appE` litE (StringL ("cannot use this constructor to create an instance of type '" <> P.show typeName <> "': ")))
          `appE` (varE (mkName "show") `appE` varE (mkName "_1"))
  match (varP $ mkName "_1") (normalB (appE (conE $ mkName "Left") errorMessage)) []

-- ConstructorName <$> decodeFieldValue d1 o1 <*> decodeFieldValue d2 o2 ...
applyDecoder :: Name -> Name -> [Int] -> ExpQ
applyDecoder _typeName cName [] = appE (varE $ mkName "pure") (conE cName)
applyDecoder typeName cName (n : ns) = do
  let cons = appE (varE $ mkName "pure") (conE cName)
  foldr (\i r -> appE (appE (varE (mkName "ap")) r) $ decodeAt i) (appE (appE (varE (mkName "ap")) cons) $ decodeAt n) (reverse ns)
  where
    decodeAt i =
      varE (mkName "decodeFieldValue") `appE` varE (mkName ("d" <> P.show i))
        `appE` (litE . StringL . P.show . dropQualified $ typeName)
        `appE` (litE . StringL . P.show . dropQualified $ cName)
        `appE` varE (mkName ("v" <> P.show i))

-- | Use a decoder to decode a field
--   The constructor name, the type where the field is inserted and the field definition
--   are used to provide better error messages
decodeFieldValue :: Decoder a -> Text -> Text -> (Maybe FieldDef, Value) -> Either Text a
decodeFieldValue d typeName constructorName (field, v) =
  case decodeValue d v of
    Right a -> pure a
    Left e -> do
      let constructor = if typeName == constructorName then "" else "(" <> constructorName <> ") "
      Left $ maybe constructor (\(fn, ft) -> constructor <> "'" <> fn <> " :: " <> ft <> "' >> ") field <> e

-- * CONSTRUCTOR DEFINITIONS

-- | Metadata for a given constructor in a data type
data ConstructorDef = ConstructorDef
  { -- | Name of the constructor
    constructorDefName :: Text,
    -- | Name of the constructor after modification with options
    constructorDefModifiedName :: Text,
    -- | Name of the constructor fields (if any are defined with names. An empty list otherwise)
    constructorDefFields :: [Text],
    -- | Names of the fields after modification with options
    constructorDefModifiedFieldNames :: [Text],
    -- | Types of the constructor fields
    constructorDefFieldsTypes :: [Text]
  }
  deriving (Eq)

instance Show ConstructorDef where
  show (ConstructorDef n _ [] _ fts) =
    toS (n <> " [" <> T.intercalate ", " fts) <> "]"
  show (ConstructorDef n _ fns _ fts) =
    toS (n <> " {" <> T.intercalate ", " ((\(fn, ft) -> fn <> " :: " <> ft) <$> zip fns fts)) <> "}"

makeConstructorDef :: Text -> [Text] -> [Text] -> ConstructorDef
makeConstructorDef constructorName fieldNames = ConstructorDef constructorName constructorName fieldNames fieldNames

-- * CONSTRUCTOR VALUES

-- | Data parsed from a given Value to be potentially used to create a constructor instance of a type
data ToConstructor = ToConstructor
  { -- | Name of the constructor to use (without modification)
    toConstructorName :: Text,
    -- | Name of the values to decode for each field of the constructor instance
    toConstructorValues :: [(Maybe FieldDef, Value)]
  }
  deriving (Eq, Show)

-- | Try to find the appropriate constructor definition encoded in the json value
--   then try to decode all its fields with decoding function
decodeFromDefinitions :: Options -> [ConstructorDef] -> Value -> (ToConstructor -> Either Text a) -> Either Text a
decodeFromDefinitions options constructorDefs value build = do
  let toConstructors = fmap build <$> makeToConstructors options constructorDefs value
  case toConstructors of
    Left e -> Left e
    Right es -> foldEither es

-- | Try to extract possible constructor values based on:
--     - the encoding options
--     - the list of constructor definitions
--     - a JSON value
--   Several alternatives can be returned for an Untagged sum encoding when there are several
--   constructor definitions
makeToConstructors :: Options -> [ConstructorDef] -> Value -> Either Text [ToConstructor]
makeToConstructors options cs value = do
  let constructors = applyOptions options <$> cs
  let isEnumeration = all (null . constructorDefFieldsTypes) constructors
  -- if the type is an enumeration
  if isEnumeration && allNullaryToStringTag options then
     case value of
      String name ->
        case find ((== name) . constructorDefModifiedName) constructors of
          Just c -> purer $ ToConstructor (constructorDefName c) []
          Nothing -> Left $ "expected one of " <> T.intercalate ", " (constructorDefModifiedName <$> constructors) <> ". Got: " <> P.show name
      other -> Left $ "expected one of " <> T.intercalate ", " (constructorDefName <$> constructors) <> ". Got: " <> encodeAsText other
  else case constructors of
    -- if there is only one constructor and tagging is not required (and nullary constructors must be tagged)
    [c] | not (tagSingleConstructors options) && not (isEnumeration && not (allNullaryToStringTag options)) ->
      pure <$> makeToConstructorFromValue options c value
    _ -> do
      maybe (pure ()) Left $ checkSumEncoding options constructors value
      case sumEncoding options of
        TaggedObject (toS -> tagFieldName) (toS -> contentsFieldName) ->
          pure <$> makeTaggedObject options tagFieldName contentsFieldName constructors value
        UntaggedValue ->
          makeUntaggedValue options constructors value
        ObjectWithSingleField ->
          pure <$> makeObjectWithSingleField options constructors value
        TwoElemArray ->
          pure <$> makeTwoElemArray options constructors value

-- | Try to find which constructor was encoded in a tagged object where the tag field encode the constructor name
--   and the values are either inline in the object or in a contents field
makeTaggedObject :: Options -> Text -> Text -> [ConstructorDef] -> Value -> Either Text ToConstructor
makeTaggedObject options tagFieldName contentsFieldName constructors value =
  tryConstructors constructors $ \c@(ConstructorDef constructorName modifiedConstructorName fieldNames modifiedFieldNames fieldTypes) ->
    case value of
      Object vs ->
        case HM.lookup tagFieldName vs of
          Just tagValue ->
            case (modifiedFieldNames, fieldNames, fieldTypes) of
              -- constructor with no fields
              ([], [], [])
                | tagValue == String modifiedConstructorName ->
                  pure $ ToConstructor constructorName []
              -- constructor with one unnamed field
              ([], [], [_])
                | tagValue == String modifiedConstructorName ->
                  case HM.lookup contentsFieldName vs of
                    Just fieldValue -> pure $ ToConstructor constructorName [(Nothing, fieldValue)]
                    Nothing -> Left $ "field " <> contentsFieldName <> " not found"
              -- constructor with one named field
              ([modifiedFieldName], [fieldName], [fieldType])
                | tagValue == String modifiedConstructorName ->
                  case HM.lookup modifiedFieldName vs of
                    Just fieldValue -> pure $ ToConstructor constructorName [(Just (fieldName, fieldType), fieldValue)]
                    Nothing -> Left $ "field " <> modifiedFieldName <> " not found"
              -- constructor with at least one named field and possibly Nothing fields
              (_, _, _)
                | tagValue == String modifiedConstructorName && omitNothingFields options && any (`elem` modifiedFieldNames) (HM.keys vs) -> do
                  let rest = HM.fromList $ filter ((/= tagFieldName) . fst) $ HM.toList vs
                  makeToConstructorFromValue options c (Object rest)
              -- constructor with several named fields
              (_, _ : _, _)
                | tagValue == String modifiedConstructorName ->
                  makeToConstructorFromValue options c value
              -- constructor with no named fields
              (_, _, _)
                | tagValue == String modifiedConstructorName && any (== contentsFieldName) (HM.keys vs) ->
                  case HM.lookup contentsFieldName vs of
                    Just contentsValue -> makeToConstructorFromValue options c contentsValue
                    _ -> Left $ "contents field not found '" <> contentsFieldName <> "'"
              (_, _, _) ->
                Left $ "failed to instantiate constructor: " <> P.show c
          Nothing ->
            Left $ "failed to instantiate constructor: " <> P.show c <> ". tag field not found: " <> tagFieldName
      _ ->
        Left $ "failed to instantiate constructor: " <> P.show c <> ". Expected an Object"

-- | Try to find which constructor was encoded in an untagged value and extract its possible values
makeUntaggedValue :: Options -> [ConstructorDef] -> Value -> Either Text [ToConstructor]
makeUntaggedValue options constructors value =
  case partitionEithers $ (\c -> makeToConstructorFromValue options c value) <$> constructors of
    (e : _, []) -> Left e
    ([], []) -> Left "no constructors"
    (_, rs) -> Right rs

-- | Try to find which constructor was encoded in an object with a single field where the field name
--   encodes the constructor name and the object values encode the constructor fields
makeObjectWithSingleField :: Options -> [ConstructorDef] -> Value -> Either Text ToConstructor
makeObjectWithSingleField options constructors value =
  tryConstructors constructors $ \c@(ConstructorDef _ modifiedConstructorName _ _ _) ->
    case value of
      Object [(tagValue, contents)]
        | tagValue == modifiedConstructorName ->
          makeToConstructorFromValue options c contents
      String v | v == modifiedConstructorName ->
        makeToConstructorFromValue options c value
      _ ->
        Left $ "failed to instantiate constructor: " <> P.show c

-- | Try to find which constructor was encoded in an array with 2 elements where the first element
--   encodes the constructor name and the other element the constructor values
makeTwoElemArray :: Options -> [ConstructorDef] -> Value -> Either Text ToConstructor
makeTwoElemArray options constructors value =
  tryConstructors constructors $ \c@(ConstructorDef _ modifiedConstructorName _ _ _) ->
    case value of
      Array [tagValue, contents]
        | tagValue == String modifiedConstructorName ->
          makeToConstructorFromValue options c contents
      String v | v == modifiedConstructorName ->
        makeToConstructorFromValue options c value
      _ ->
        Left $ "failed to instantiate constructor: " <> P.show c

-- | Check if the sum encoding structure looks correct
--   This requires the whole list of constructor definitions
checkSumEncoding :: Options -> [ConstructorDef] -> Value -> Maybe Text
checkSumEncoding options constructors value = do
  let constructorModifiedNames = constructorDefModifiedName <$> constructors
  case sumEncoding options of
    TaggedObject (toS -> tagFieldName) _contentsFieldName ->
      case value of
        Object vs ->
          case HM.lookup tagFieldName vs of
            Nothing -> Just $ "tag field '" <> tagFieldName <> "' not found"
            Just (String tagValue)
              | tagValue `elem` constructorModifiedNames ->
                Nothing
            Just v ->
              unexpectedConstructor constructorModifiedNames v
        _ -> Just "expected an Object for a TaggedObject sum encoding"
    UntaggedValue ->
      Nothing
    ObjectWithSingleField ->
      case value of
        Object [(tagValue, _)] ->
          if tagValue `elem` constructorModifiedNames
            then Nothing
            else unexpectedConstructor constructorModifiedNames (String tagValue)
        String v | v `elem` constructorModifiedNames -> Nothing
        _ -> Just "expected an Object for an ObjectWithSingleField sum encoding"
    TwoElemArray ->
      case value of
        Array [String tagValue, _] ->
          if tagValue `elem` constructorModifiedNames
            then Nothing
            else unexpectedConstructor constructorModifiedNames (String tagValue)
        String v | v `elem` constructorModifiedNames -> Nothing
        _ -> Just "expected an Array with 2 elements for an TwoElemArray sum encoding"
  where
    unexpectedConstructor :: [Text] -> Value -> Maybe Text
    unexpectedConstructor expected (String c) = Just $ "expected the tag field to be one of: " <> T.intercalate ", " expected <> ", found: " <> c
    unexpectedConstructor expected other = Just $ "expected the tag field to be one of: " <> T.intercalate ", " expected <> ", found: " <> T.decodeUtf8 (BL.toStrict $ encode other)

-- | Apply at runtime options to a constructor definition in order to be
--   able to match field definitions in the decoded json value
applyOptions :: Options -> ConstructorDef -> ConstructorDef
applyOptions options (ConstructorDef constructorName _ fieldNames _ fieldTypes) =
  ConstructorDef
    constructorName
    (toS . constructorTagModifier options . toS $ constructorName)
    fieldNames
    (toS . fieldLabelModifier options . toS <$> fieldNames)
    fieldTypes

-- | For a given constructor definition extract all the required fields from a json value
makeToConstructorFromValue :: Options -> ConstructorDef -> Value -> Either Text ToConstructor
-- no field
makeToConstructorFromValue _options (ConstructorDef constructorName modifiedConstructorName [] _ []) value =
  case value of
    String v ->
      if v == modifiedConstructorName
        then pure $ ToConstructor constructorName []
        else Left $ "incorrect constructor name, expected: " <> modifiedConstructorName <> ". Got: " <> v
    _ ->
        Left $ "incorrect constructor name, expected: " <> modifiedConstructorName <> ". Got: " <> encodeAsText value
-- one field, no field name
makeToConstructorFromValue _options (ConstructorDef constructorName _ [] _ [_]) value =
  pure $ ToConstructor constructorName [(Nothing, value)]
-- one field, one field name
makeToConstructorFromValue options (ConstructorDef constructorName _ [f] [mf] [t]) value =
  if unwrapUnaryRecords options
    then pure $ ToConstructor constructorName [(Nothing, value)]
    else case value of
      Object [(f', v)] | f' == mf -> pure $ ToConstructor constructorName [(Just (f, t), v)]
      _ -> Left $ "field '" <> mf <> "' not found" <> (if mf == f then "" else " (to create field '" <> f <> "')")
-- several fields
makeToConstructorFromValue options (ConstructorDef constructorName _ _ modifiedFieldNames fieldTypes) value =
  case value of
    Object vs -> do
      let fieldsNotFound = modifiedFieldNames \\ HM.keys vs
      if not (omitNothingFields options) && not (null fieldsNotFound)
        then Left $ case fieldsNotFound of
          [f] -> "field '" <> f <> "' not found"
          fs -> "fields  not found: " <> T.intercalate "," fs
        else do
          let fields = zip modifiedFieldNames fieldTypes
          pure $ ToConstructor constructorName $ mapMaybe (getValue vs) fields
      where
        getValue :: Object -> (Text, Text) -> Maybe (Maybe FieldDef, Value)
        getValue actualFields (fieldName, fieldType) =
          case HM.lookup fieldName actualFields of
            Just v -> Just (Just (fieldName, fieldType), v)
            Nothing ->
              if omitNothingFields options && "Maybe" `T.isPrefixOf` fieldType
                then Just (Just (fieldName, fieldType), Null)
                else Nothing
    Array vs -> pure $ ToConstructor constructorName ((Nothing,) <$> toList vs)
    _ -> pure $ ToConstructor constructorName [(Nothing, value)]

-- | Field name + field type
type FieldDef = (Text, Text)

-- | Return a textual description of a json value
jsonTypeOf :: Value -> Text
jsonTypeOf (Object _) = "an Object"
jsonTypeOf (Array _) = "an Array"
jsonTypeOf (String _) = "a String"
jsonTypeOf (Number _) = "a Number"
jsonTypeOf (Bool _) = "a Bool"
jsonTypeOf Null = "Null"

tryConstructors :: [ConstructorDef] -> (ConstructorDef -> Either Text ToConstructor) -> Either Text ToConstructor
tryConstructors constructors f = foldEither $ f <$> constructors

foldEither :: [Either Text c] -> Either Text c
foldEither es = do
  let (ls, rs) = partitionEithers es
  case (ls, rs) of
    ([], []) -> Left "no results"
    (errors, []) -> Left (T.intercalate " ->> " errors)
    (_, r : _) -> Right r

encodeAsText :: (ToJSON a) => a -> Text
encodeAsText = T.decodeUtf8 . BL.toStrict . encode
