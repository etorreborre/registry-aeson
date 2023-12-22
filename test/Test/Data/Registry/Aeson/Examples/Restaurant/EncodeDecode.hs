{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Aeson.Examples.Restaurant.EncodeDecode where

import Test.Data.Registry.Aeson.Examples.Restaurant.Model
import Data.Aeson hiding (decode, encode)
import Data.Aeson qualified as A
import Data.Registry
import Data.Registry.Aeson.Encoder
import Data.Registry.Aeson.Decoder
import Data.Text.Encoding qualified as T
import Protolude
import Test.Tasty.Hedgehogx hiding (either, maybe, text)
import Data.ByteString.Lazy qualified as BL (fromStrict, toStrict)
import Test.Data.Registry.Aeson.EncoderSpec (checkValue)

encoders :: Registry _ _
encoders =
       fun tableEncoder
    <: fun singleTableEncoder
    <: $(makeEncoder ''Natural)
    <: jsonEncoder @Integer
    <: val (defaultOptions {unwrapUnaryRecords = True})
    <: defaultEncoderOptions

singleTableEncoder :: Encoder Natural -> Encoder SingleTable
singleTableEncoder nat = Encoder \(SingleTable c m) -> do
  let value =
        A.object [
          "capacity" .= encodeValue nat c,
          "minimalReservation" .= encodeValue nat m
        ]
  (value, toEncoding value)

tableEncoder :: Encoder Natural -> Encoder SingleTable -> Encoder Table
tableEncoder nat st = Encoder \case
  Single t -> (A.object ["single" .= encodeValue st t], undefined)
  Communal c -> (A.object ["communal" .= encodeValue nat c], undefined)

decoders :: Registry _ _
decoders =
       $(makeDecoder ''Table)
    <: $(makeDecoder ''SingleTable)
    <: $(makeDecoder ''Natural)
    <: jsonDecoder @Integer
    <: val (defaultOptions {unwrapUnaryRecords = True})
    <: defaultDecoderOptions

test_encode = test "encode" $ do
  checkEncoding (Natural 123) "123"
  let singleTable = (SingleTable (Natural 5) (Natural 2))
  checkEncoding singleTable "{\"minimalReservation\":2,\"capacity\":5}"
  checkEncoding (Single singleTable) "{\"single\":{\"minimalReservation\":2,\"capacity\":5}}"

checkEncoding :: forall a. (Typeable a) => a -> Text -> PropertyT IO ()
checkEncoding a expected = withFrozenCallStack $ do
  let encoder = make @(Encoder a) encoders
  let asValue = BL.toStrict . A.encode $ encodeValue encoder a
  annotate "the encoded Value must be the expected value"
  checkValue asValue (T.encodeUtf8 expected)
