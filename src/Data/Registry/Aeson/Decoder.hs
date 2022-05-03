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
import Data.List (nub)
import Data.Registry
import Data.Registry.Aeson.TH
import Data.Registry.Internal.Types hiding (Value)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude hiding (Type)
import Prelude (String)

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
    Left e -> Left $ "cannot unpack the bytestring as a Value: " <> show e <> ". The bytestring is: " <> show bs
    Right v ->
      case decodeValue d v of
        Right a -> pure a
        Left e -> Left $ toS e <> ". Cannot decode " <> toS (showType @a) <> " from the Value: " <> show v

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
  other -> Left . toS $ "not a pair of " <> showType @a <> "," <> showType @b <> ": " <> show other

-- | Add a Maybe (a, b, c) to a registry of decoders
--   usage: decoders = decodeTripleOf @a @b @c <: otherDecoders
--   the list of otherDecoders must contain a Decoder a, a Decoder b and a Decoder c
--   otherwise there will be a compilation error
decodeTripleOf :: forall a b c. (Typeable a, Typeable b, Typeable c) => Typed (Decoder a -> Decoder b -> Decoder c -> Decoder (a, b, c))
decodeTripleOf = fun (tripleOfDecoder @a @b @c)

tripleOfDecoder :: forall a b c. (Typeable a, Typeable b, Typeable c) => Decoder a -> Decoder b -> Decoder c -> Decoder (a, b, c)
tripleOfDecoder (Decoder a) (Decoder b) (Decoder c) = Decoder $ \case
  Array [oa, ob, oc] -> (,,) <$> a oa <*> b ob <*> c oc
  other -> Left . toS $ "not a triple of " <> showType @a <> "," <> showType @b <> "," <> showType @c <> ": " <> show other

-- | Add a Decoder [a] to a registry of decoders
--   usage: decoders = decodeListOf @a <: otherDecoders
--   the list of otherDecoders must contain a Decoder a
--   otherwise there will be a compilation error
decodeListOf :: forall a. (Typeable a) => Typed (Decoder a -> Decoder [a])
decodeListOf = fun (listOfDecoder @a)

listOfDecoder :: forall a. (Typeable a) => Decoder a -> Decoder [a]
listOfDecoder (Decoder a) = Decoder $ \case
  Object os -> for (toList os) a
  other -> Left . toS $ "not a list of " <> showType @a <> ": " <> show other

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
  other -> Left . toS $ "not a list of " <> showType @a <> ": " <> show other

showType :: forall a. (Typeable a) => String
showType = show (typeRep (Proxy :: Proxy a))

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
      qReport True ("can only create decoders for an ADT, got: " <> show other)
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
--   \opts d1 d2 d3 -> Decoder $ \v -> do
--     case makeToConstructor opts [Constructor "T1" ["f1", "f2"], Constructor "T2" []] v of
--       ToConstructor "T1" [v1, v2]-> T1 <$> d1 v1 <*> d2 v2 ...
--       ToConstructor "T2" [v1, v2]-> T2 <$> d1 v1 <*> d3 v2 ...
--       other -> Left ("cannot decode " <> valueToText v)
makeConstructorsDecoder :: Name -> [Con] -> ExpQ
makeConstructorsDecoder typeName cs = do
  ts <- nub . join <$> for cs typesOf
  let decoderParameters = sigP (varP (mkName "os")) (conT $ mkName "Options") : ((\(t, n) -> sigP (varP (mkName $ "d" <> show n)) (appT (conT $ mkName "Decoder") (pure t))) <$> zip ts [0 ..])
  -- makeToConstructor os [Constructor "T1" ["f1", "f2"], Constructor "T2" []] v
  let paramP = varP (mkName "v")
  constructorDefs <- for cs $ \c -> do
    cName <- dropQualified <$> nameOf c
    fields <- fmap (litE . StringL . show . dropQualified) <$> fieldsOf c
    fieldTypes <- fmap (litE . StringL . show) <$> typesOf c
    varE (mkName "makeConstructorDef") `appE` (litE . StringL $ show cName) `appE` listE fields `appE` listE fieldTypes
  let paramE = varE (mkName "makeToConstructor") `appE` varE (mkName "os") `appE` listE (pure <$> constructorDefs) `appE` varE (mkName "v")
  let matchClauses = makeMatchClause ts <$> cs
  let errorClause = makeErrorClause typeName
  let decoded = caseE paramE (matchClauses <> [errorClause])
  lamE decoderParameters (appE (conE (mkName "Decoder")) (lamE [paramP] decoded))

-- | Return an error if a value dose not have the expected type
--   other -> Error (mconcat ["not a valid ", show typeName, ": ", show other])
makeErrorClause :: Name -> MatchQ
makeErrorClause typeName = do
  let errorMessage =
        appE (varE $ mkName "mconcat") $
          listE
            [ litE (StringL "not a valid "),
              litE (StringL $ show typeName),
              litE (StringL ": "),
              appE (varE $ mkName "show") (varE $ mkName "_1")
            ]
  match (varP $ mkName "_1") (normalB (appE (conE $ mkName "Left") errorMessage)) []

-- | Decode the nth constructor of a data type
--    ToConstructor "T1" [v1, v2]-> T1 <$> d1 v1 <*> d2 v2 ...
makeMatchClause :: [Type] -> Con -> MatchQ
makeMatchClause allTypes c = do
  ts <- typesOf c
  constructorTypes <- fmap snd <$> indexConstructorTypes allTypes ts
  cName <- dropQualified <$> nameOf c
  let fieldsP = listP $ (\i -> varP $ mkName ("v" <> show i)) <$> constructorTypes
  match
    (conP (mkName "Right") [conP (mkName "ToConstructor") [litP (StringL . show $ cName), fieldsP]])
    (normalB (applyDecoder cName constructorTypes))
    []

-- ConstructorName <$> decode d1 o1 <*> decode d2 o2 ...
applyDecoder :: Name -> [Int] -> ExpQ
applyDecoder cName [] = appE (varE $ mkName "pure") (conE cName)
applyDecoder cName (n : ns) = do
  let cons = appE (varE $ mkName "pure") (conE cName)
  foldr (\i r -> appE (appE (varE (mkName "ap")) r) $ decodeAt i) (appE (appE (varE (mkName "ap")) cons) $ decodeAt n) (reverse ns)
  where
    decodeAt i = appE (appE (varE $ mkName "decodeValue") (varE $ mkName ("d" <> show i))) (varE $ mkName ("v" <> show i))

decodeField :: Decoder a -> Text -> Value -> Either Text a
decodeField (Decoder d) name (Object ls) =
  case HM.lookup name ls of
    Just v -> d v
    Nothing -> Left $ "field '" <> name <> "' not found in: " <> show ls
decodeField _ name o =
  Left $ "field '" <> name <> "' not found in: " <> show o

-- | Data parsed from a given Value to be used to create an instance of a type
data ToConstructor = ToConstructor
  { -- | Name of the constructor to use (without modification)
    toConstructorName :: Text,
    -- | Name of the values to decode for each field of the constructor instance
    toConstructorValues :: [Value]
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

makeConstructorDef :: Text -> [Text] -> [Text] -> ConstructorDef
makeConstructorDef constructorName fieldNames fieldTypes = ConstructorDef constructorName constructorName fieldNames fieldNames fieldTypes

-- | Parse the values of a given constructor from:
--     - the encoding options
--     - the list of constructor definitions
--     - a JSON value
makeToConstructor :: Options -> [ConstructorDef] -> Value -> Either Text ToConstructor
makeToConstructor options constructors value =
  if all (null . constructorDefFieldsTypes) constructors && allNullaryToStringTag options
    then case value of
      String name ->
        case find (== name) $ constructorDefName <$> constructors of
          Just n -> pure $ ToConstructor n []
          Nothing -> Left $ "expected one of " <> T.intercalate ", " (constructorDefName <$> constructors) <> ". Got: " <> show name
      other -> Left $ "expected one of " <> T.intercalate ", " (constructorDefName <$> constructors) <> ". Got: " <> show other
    else case constructors of
      [c]
        | not (tagSingleConstructors options) ->
          pure $ makeToConstructorFromValue options c value
      _ ->
        go constructors []
  where
    go :: [ConstructorDef] -> [Text] -> Either Text ToConstructor
    go [] errors = Left $ T.intercalate "\n" errors
    go (c : cs) errors =
      case tryMakeToConstructor options (applyOptions options c) value of
        Right tc -> Right tc
        Left e -> go cs (errors <> [e])

tryMakeToConstructor :: Options -> ConstructorDef -> Value -> Either Text ToConstructor
tryMakeToConstructor options c@(ConstructorDef constructorName modifiedConstructorName _ modifiedFieldNames fieldTypes) value =
  case sumEncoding options of
    TaggedObject (toS -> tagFieldName) (toS -> contentsFieldName) ->
      case value of
        Object vs ->
          case HM.lookup tagFieldName vs of
            Just tagValue ->
              case (modifiedFieldNames, fieldTypes) of
                -- constructor with no fields
                ([], [])
                  | tagValue == String modifiedConstructorName ->
                    pure $ ToConstructor constructorName []
                -- constructor with one unnamed field
                ([], [_])
                  | tagValue == String modifiedConstructorName ->
                    case HM.lookup contentsFieldName vs of
                      Just fieldValue -> pure $ ToConstructor constructorName [fieldValue]
                      Nothing -> Left $ "failed to instantiate constructor: " <> show c <> ". Field " <> contentsFieldName <> " not found"
                -- constructor with one named field
                ([modifiedFieldName], [_])
                  | tagValue == String modifiedConstructorName ->
                    case HM.lookup modifiedFieldName vs of
                      Just fieldValue -> pure $ ToConstructor constructorName [fieldValue]
                      Nothing -> Left $ "failed to instantiate constructor: " <> show c <> ". Field " <> modifiedFieldName <> " not found"
                -- constructor with at least one named field and possibly Nothing fields
                (_, _)
                  | tagValue == String modifiedConstructorName && omitNothingFields options && any (\f -> f `elem` modifiedFieldNames) (HM.keys vs) ->
                    case filter ((/= tagFieldName) . fst) $ HM.toList vs of
                      [(fieldName, fieldValue)] -> pure $ makeToConstructorFromValue options c (Object [(fieldName, fieldValue)])
                      _ -> Left $ "failed to instantiate constructor: " <> show c
                -- constructor with several fields
                (_, _)
                  | tagValue == String modifiedConstructorName && any (== contentsFieldName) (HM.keys vs) ->
                    case HM.lookup contentsFieldName vs of
                      Just contentsValue -> pure $ makeToConstructorFromValue options c contentsValue
                      _ -> Left $ "failed to instantiate constructor: " <> show c
                (_, _) ->
                  Left $ "failed to instantiate constructor: " <> show c
            Nothing ->
              Left $ "failed to instantiate constructor: " <> show c
        _ ->
          Left $ "failed to instantiate constructor: " <> show c <> ". Expected an Object"
    UntaggedValue ->
      pure $ makeToConstructorFromValue options c value
    ObjectWithSingleField ->
      case value of
        Object [(tagValue, contents)]
          | tagValue == modifiedConstructorName ->
            pure $ makeToConstructorFromValue options c contents
        _ ->
          Left $ "failed to instantiate constructor: " <> show c
    TwoElemArray ->
      case value of
        Array [tagValue, contents]
          | tagValue == String modifiedConstructorName ->
            pure $ makeToConstructorFromValue options c contents
        _ ->
          Left $ "failed to instantiate constructor: " <> show c

applyOptions :: Options -> ConstructorDef -> ConstructorDef
applyOptions options (ConstructorDef constructorName _ fieldNames _ fieldTypes) =
  ConstructorDef
    constructorName
    (toS . constructorTagModifier options . toS $ constructorName)
    fieldNames
    (toS . fieldLabelModifier options . toS <$> fieldNames)
    fieldTypes

makeToConstructorFromValue :: Options -> ConstructorDef -> Value -> ToConstructor
makeToConstructorFromValue _options (ConstructorDef constructorName _ [] [] _) value =
  case value of
    Object vs -> ToConstructor constructorName . reverse $ HM.elems vs
    Array vs -> ToConstructor constructorName (toList vs)
    _ -> ToConstructor constructorName [value]
makeToConstructorFromValue options (ConstructorDef constructorName _ _ modifiedFieldNames fieldTypes) value =
  case value of
    Object vs -> do
      let fields = zip modifiedFieldNames fieldTypes
      ToConstructor constructorName $ mapMaybe (getValue vs) fields
      where
        getValue :: Object -> (Text, Text) -> Maybe Value
        getValue actualFields (fieldName, fieldType) =
          case HM.lookup fieldName actualFields of
            Just v -> Just v
            Nothing ->
              if omitNothingFields options && "AppT (ConT GHC.Maybe.Maybe)" `T.isPrefixOf` fieldType
                then Just Null
                else Nothing
    Array vs -> ToConstructor constructorName (toList vs)
    _ -> ToConstructor constructorName [value]
