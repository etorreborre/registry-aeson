{-# OPTIONS_GHC -Wno-type-defaults #-}

module Data.Registry.Aeson.TH.Encoder where

import Control.Monad.Fail
import Data.List (nub)
import Data.Registry.Aeson.TH.TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude as P hiding (Type)

{-
  This module uses TemplateHaskell to extract enough type information to be able to
  build an Encoder based on configuration options
-}

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

-- \(o::Options) (ce::ConstructorEncoder) (e0::Encoder A0) (e1::Encoder A1) ... -> Encoder $ \a ->
--   case a of
--     T1 a0 a1 ... -> encodeConstructor ce o (FromConstructor names types "T1" fieldNames [encode e0 a0, encode e1 a1, ...])
--     T2 a0 a4 ... -> encodeConstructor ce o (FromConstructor names types "T2" fieldNames [encode e0 a0, encode e4 a4, ...])
makeConstructorsEncoder :: [Con] -> ExpQ
makeConstructorsEncoder cs = do
  -- get the types of all the fields of all the constructors
  ts <- nub . join <$> for cs typesOf
  constructorsNames <- for cs nameOf
  let options = sigP (varP (mkName "os")) (conT $ mkName "Options")
  let constructorEncoder = sigP (varP (mkName "ce")) (conT $ mkName "ConstructorEncoder")
  let encoderParameters = options : constructorEncoder : ((\(t, n) -> sigP (varP (mkName $ "e" <> show n)) (appT (conT $ mkName "Encoder") (pure t))) <$> zip ts [0 ..])
  matchClauses <- for cs $ makeMatchClause constructorsNames ts
  lamE encoderParameters (appE (conE (mkName "Encoder")) (lamCaseE (pure <$> matchClauses)))

-- | Make the match clause for a constructor given
--    - the list of all the encoder types
--    - the constructor name
--    - the constructor index in the list of all the constructors for the encoded data type
--   T1 a0 a1 ... -> encodeConstructor ce o (FromConstructor names types cName fieldNames values)
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
        varE (mkName "encodeConstructor")
          `appE` varE (mkName "ce")
          `appE` varE (mkName "os")
          `appE` (conE (mkName "FromConstructor") `appE` names `appE` types `appE` litE (StringL $ show cName) `appE` fieldNames `appE` values)
  match params (normalB encoded) []
