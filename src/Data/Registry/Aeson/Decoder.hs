{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-
  A Decoder is used to decode a Aeson Object into a specific data type
  This module provides several functions to create decoders and assemble them into a registry of encoders.
-}
module Data.Registry.Aeson.Decoder where

import Control.Monad.Fail
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.List (nub)
import Data.Registry
import Data.Registry.Aeson.TH
import Data.Registry.Internal.Types hiding (Value)
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
decodeByteString :: forall a. (Typeable a) => Decoder a -> ByteString -> Either Text a
decodeByteString d bs =
  case eitherDecodeStrict bs of
    Left e -> Left $ "cannot unpack the bytestring as a Value: " <> show e <> ". The bytestring is: " <> show bs
    Right v ->
      case decodeValue d v of
        Right a -> pure a
        Left e -> Left $ "Error: " <> toS e <> ". Cannot decode " <> toS (showType @a) <> " from the Value: " <> show v

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
    TyConI (NewtypeD _context _name _typeVars _kind (RecC constructor [(_, _, other)]) _deriving) -> do
      -- \(a::Decoder OldType) -> fmap NewType d
      lamE [sigP (varP $ mkName "d") (appT (conT $ mkName "Decoder") (pure other))] (appE (appE (varE $ mkName "fmap") (conE . mkName $ show constructor)) (varE $ mkName "d"))
    TyConI (NewtypeD _context _name _typeVars _kind (NormalC constructor [(_, other)]) _deriving) -> do
      -- \(a::Decoder OldType) -> fmap NewType d
      lamE [sigP (varP $ mkName "d") (appT (conT $ mkName "Decoder") (pure other))] (appE (appE (varE $ mkName "fmap") (conE . mkName $ show constructor)) (varE $ mkName "d"))
    TyConI (DataD _context _name _typeVars _kind constructors _deriving) -> do
      case constructors of
        [] -> do
          qReport True "can not make an Decoder for an empty data type"
          fail "decoders creation failed"
        [c] -> makeConstructorDecoder typeName c
        _ -> makeConstructorsDecoder typeName constructors
    other -> do
      qReport True ("can only create decoders for an ADT, got: " <> show other)
      fail "decoders creation failed"

-- | Make a Decoder for a single Constructor, where each field of the constructor is encoded as an element of an Object
makeConstructorDecoder :: Name -> Con -> ExpQ
makeConstructorDecoder typeName c = do
  ts <- typesOf c
  fields <- fieldsOf c
  cName <- nameOf c
  let decoderParameters = (\(t, n) -> sigP (varP (mkName $ "d" <> show n)) (appT (conT $ mkName "Decoder") (pure t))) <$> zip ts [0 ..]
  let paramP = varP (mkName "o")
  let paramE = varE (mkName "o")
  let matchClause =
        match
          (varP (mkName "anObject"))
          (normalB (applyDecoder cName [0 .. length ts - 1] fields))
          []

  let decoded = caseE paramE [matchClause, makeErrorClause typeName]

  -- (\(d1::Decoder Type1) (d2::Decoder Type2) ... -> Decoder (\case
  --     Array (toList -> [o1, o2, ...]) -> Constructor <$> decode d1 o1 <*> decode d2 o2 ...))
  --     other -> Error ("not a valid " <> constructorType <> ": " <> show other)
  lamE decoderParameters (appE (conE (mkName "Decoder")) (lamE [paramP] decoded))

-- | Make a Decoder for a each Constructor of a data type:
--     - each constructor is specified by an Array [Number n, o1, o2, ...]
--     - n specifies the number of the constructor
--     - each object in the array represents a constructor field
makeConstructorsDecoder :: Name -> [Con] -> ExpQ
makeConstructorsDecoder typeName cs = do
  ts <- nub . join <$> for cs typesOf
  let decoderParameters = (\(t, n) -> sigP (varP (mkName $ "d" <> show n)) (appT (conT $ mkName "Decoder") (pure t))) <$> zip ts [0 ..]
  let paramP = varP (mkName "o")
  let paramE = varE (mkName "o")
  let matchClauses = uncurry (makeMatchClause ts) <$> zip cs [0 ..]
  let errorClause = makeErrorClause typeName
  let decoded = caseE paramE (matchClauses <> [errorClause])

  -- (\(d1::Decoder Type1) (d2::Decoder Type2) ... -> Decoder (\case
  --     Array (toList -> [Number n, o1, o2, ...]) -> Constructor <$> decode d1 o1 <*> decode d2 o2 ...))
  --     other -> Error ("not a valid " <> constructorType <> ": " <> show other)
  lamE decoderParameters (appE (conE (mkName "Decoder")) (lamE [paramP] decoded))

-- | Return an error if an object is not an Array as expected
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
makeMatchClause :: [Type] -> Con -> Integer -> MatchQ
makeMatchClause allTypes c constructorIndex = do
  ts <- typesOf c
  fields <- fieldsOf c
  print (ts, fields)
  constructorTypes <- fmap snd <$> indexConstructorTypes allTypes ts
  cName <- nameOf c
  let paramsP = conP (mkName "Number") [litP (IntegerL constructorIndex)] : ((\n -> varP $ mkName $ "o" <> show n) <$> constructorTypes)
  match
    (conP (mkName "Array") [viewP (varE (mkName "toList")) (listP paramsP)])
    (normalB (applyDecoder cName constructorTypes fields))
    []

-- ConstructorName <$> decode d1 o1 <*> decode d2 o2 ...
applyDecoder :: Name -> [Int] -> [Name] -> ExpQ
applyDecoder cName [] [] = appE (varE $ mkName "pure") (conE cName)
applyDecoder cName (n : ns) (f : fs) = do
  let cons = appE (varE $ mkName "pure") (conE cName)
  foldr (\(i, fi) r -> appE (appE (varE (mkName "ap")) r) $ decodeAt i fi) (appE (appE (varE (mkName "ap")) cons) $ decodeAt n f) (reverse (zip ns fs))
  where
    decodeAt i fi = appE (appE (appE (varE $ mkName "decodeField") (varE $ mkName ("d" <> show i))) (litE . StringL . show $ fi)) (varE $ mkName "anObject")
-- this should really not happen
applyDecoder cName ns fs = fail $ "there should be the same number of types and fields for " <> show cName <> ". Remaining types: " <> show ns <> ". Remaining fields: " <> show fs

decodeField :: Decoder a -> Text -> Value -> Either Text a
decodeField (Decoder d) name (Object ls) =
  case HM.lookup name ls of
    Just v -> d v
    Nothing -> Left $ "field '" <> name <> "' not found in: " <> show ls
decodeField _ name o =
  Left $ "field '" <> name <> "' not found in: " <> show o

{-

\opts e1 e2 e3 -> Encoder $ \a ->
  makeEncoding opts `appE` (FromConstructor names name [
    (fieldName1, e1 $ field1 a),
    (fieldName2, e2 $ field2 a),
    (fieldName3, e3 $ field3 a)]
    )


makeFromConstructor :: Con -> ExpQ

data FromConstructor = FromConstructor {
  fromConstructorNames :: [Text],
  fromConstructorName :: Text,
  fromConstructorValues :: [(Text, Value)]
}

makeEncoding :: Options -> FromConstructor -> (Value, Encoding)

---

makeToConstructor :: Options -> Value -> ToConstructor


\opts d1 d2 d3 -> Decoder $ \v ->
  case makeToConstructor opts v of
    ToConstructor name [(f1, v1), (f2, v2)] -> do
      c <- $(findConstructor name)
      a1 <- d1 v1
      a2 <- d1 v2
      pure $ c `appE` a1 `appE` a2 ...
    other -> Left "boom"


data ToConstructor = ToConstructor {
  toConstructorName :: Text,
  toConstructorValues :: [(Text, Value)]
}



1. if all nullary constructors

  if allNullaryToStringTag os then
    case v of
      String c1 -> Right C1
      String c2 -> Right C2
      other -> Left $ "unknown constructor:" <> show other
  else \v ->
    case sumEncoding os of
      TaggedObject tagFieldName contentsFieldName -> do
        constructorName <- getFieldValueAsString tagFieldName v
        case constructorName of

        contents <- getFieldValue contentsFieldName v
        $(decodeConstructor (constructorTagModifier os $ constructorName) contents)
      UntaggedValue ->
      ObjectWithSingleField ->
      TwoElemArray -> Right C2
      other -> Left $ "unknown constructor:" <> show other



\(os::Options) (d0: Decoder a) (Decoder d1) (Decoder d2) -> \v ->
-}
