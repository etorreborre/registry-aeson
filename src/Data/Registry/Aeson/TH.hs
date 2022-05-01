{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-
  TemplateHaskell functions
-}

module Data.Registry.Aeson.TH where

import Control.Monad.Fail
import Data.List (elemIndex)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude hiding (Type)

indexConstructorTypes :: [Type] -> [Type] -> Q [(Type, Int)]
indexConstructorTypes allTypes constructorTypes =
  for constructorTypes $ \t ->
    case elemIndex t allTypes of
      Just n -> pure (t, n)
      Nothing -> fail $ "the type " <> show t <> " cannot be found in the list of all types " <> show allTypes

-- | Get the types of all the fields of a constructor
typesOf :: Con -> Q [Type]
typesOf (NormalC _ types) = pure (snd <$> types)
typesOf (RecC _ types) = pure $ (\(_, _, t) -> t) <$> types
typesOf other = do
  qReport True ("we can only create encoders / decoders for normal constructors and records, got: " <> show other)
  fail "encoders / decoders creation failed"

-- | Get the name of a constructor
nameOf :: Con -> Q Name
nameOf (NormalC n _) = pure n
nameOf (RecC n _) = pure n
nameOf other = do
  qReport True ("we can only create encoders / decoders for normal constructors and records, got: " <> show other)
  fail "encoders / decoders creation failed"

-- | Get the list of names of a constructor
fieldsOf :: Con -> Q [Name]
fieldsOf (NormalC _ _) = pure []
fieldsOf (RecC _ types) = pure $ (\(f, _, _) -> f) <$> types
fieldsOf other = do
  qReport True ("we can only create encoders / decoders for normal constructors and records, got: " <> show other)
  fail "encoders / decoders creation failed"

-- | Remove the module name from a qualified name
dropQualified :: Name -> Name
dropQualified name = maybe name (mkName . toS) (lastMay (T.splitOn "." (show name)))
