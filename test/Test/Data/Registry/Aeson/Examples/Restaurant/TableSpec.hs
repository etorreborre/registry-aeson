{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Aeson.Examples.Restaurant.TableSpec where

import Data.Aeson hiding (decode, encode, object)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap
import Data.ByteString.Lazy qualified as BL (toStrict)
import Data.Map (assocs, (!?))
import Data.Registry
import Data.Registry.Aeson.Decoder
import Data.Registry.Aeson.Encoder
import Data.Text.Encoding qualified as T
import Protolude
import Test.Data.Registry.Aeson.EncoderSpec (checkValue)
import Test.Data.Registry.Aeson.Examples.Restaurant.Model
import Test.Tasty.Hedgehogx hiding (either, maybe, text)

-- This example is taken from this blog post: https://blog.ploeh.dk/2023/12/11/serializing-restaurant-tables-in-haskell
test_encode_table = test "encode restaurant tables" $ do
  checkEncoding (Natural 123) "123"
  let singleTable = (SingleTable (Natural 5) (Natural 2))
  checkEncoding singleTable "{\"minimalReservation\":2,\"capacity\":5}"
  checkEncoding (Single singleTable) "{\"single\":{\"minimalReservation\":2,\"capacity\":5}}"
  checkEncoding (Communal (Natural 5)) "{\"communal\":{\"capacity\":5}}"

test_decode_table = test "decode restaurant tables" $ do
  checkDecoding "123" (Natural 123)
  let singleTable = (SingleTable (Natural 5) (Natural 2))
  checkDecoding "{\"minimalReservation\":2,\"capacity\":5}" singleTable
  checkDecoding "{\"single\":{\"minimalReservation\":2,\"capacity\":5}}" (Single singleTable)
  checkDecoding "{\"communal\":{\"capacity\":5}}" (Communal (Natural 5))

-- HELPERS

encoders :: Registry _ _
encoders =
  fun tableEncoder
    <: fun singleTableEncoder
    <: $(makeEncoder ''Natural)
    <: jsonEncoder @Integer
    <: val (defaultOptions {unwrapUnaryRecords = True})
    <: defaultEncoderOptions

singleTableEncoder :: Encoder Natural -> Encoder SingleTable
singleTableEncoder nat = Encoder \(SingleTable c m) ->
  object
    [ pair "capacity" (encode nat c),
      pair "minimalReservation" (encode nat m)
    ]

tableEncoder :: Encoder Natural -> Encoder SingleTable -> Encoder Table
tableEncoder nat st = Encoder \case
  Single t -> single "single" (encode st t)
  Communal c -> single "communal" (single "capacity" (encode nat c))

singleTableDecoder :: Decoder Natural -> Decoder SingleTable
singleTableDecoder nat = Decoder \case
  Object fields -> case assocs $ toMap fields of
    [("capacity", capacity), ("minimalReservation", reservation)] ->
      SingleTable <$> decodeValue nat capacity <*> decodeValue nat reservation
    _ -> Left "cannot decode a SingleTable, the JSON value should have only 2 keys: 'capacity' and 'minimalReservation'"
  _ ->
    Left "cannot decode a SingleTable, the JSON value is not an object"

tableDecoder :: Decoder Natural -> Decoder SingleTable -> Decoder Table
tableDecoder nat st = Decoder $ \case
  Object fields -> case assocs $ toMap fields of
    [("communal", Object capacity)] ->
        case toMap capacity !? "capacity" of
          Just n -> Communal <$> decodeValue nat n
          Nothing -> Left "missing a capacity field"
    [("single", table)] -> Single <$> decodeValue st table
    _ -> Left "cannot decode the table, the JSON value should have only one key: 'communal' or 'single'"
  _ ->
    Left "cannot decode the table, the JSON value is not an object"

decoders :: Registry _ _
decoders =
       fun tableDecoder
    <: fun singleTableDecoder
    <: $(makeDecoder ''Natural)
    <: jsonDecoder @Integer
    <: val (defaultOptions {unwrapUnaryRecords = True})
    <: defaultDecoderOptions

-- Encode a value and check that it corresponds to the expected text
checkEncoding :: forall a. (Typeable a) => a -> Text -> PropertyT IO ()
checkEncoding a expected = withFrozenCallStack $ do
  let encoder = make @(Encoder a) encoders
  let asValue = BL.toStrict . A.encode $ encodeValue encoder a
  annotate "the encoded Value must be the expected value"
  checkValue asValue (T.encodeUtf8 expected)

-- Decode some JSON as text and check that it corresponds to the expected value
checkDecoding :: forall a. (Typeable a, Eq a, Show a) => Text -> a -> PropertyT IO ()
checkDecoding t expected = withFrozenCallStack $ do
  let decoder = make @(Decoder a) decoders
  let actual :: Either Text a = decodeText decoder t
  annotate "the decoded Value must be the expected value"
  actual === Right expected
