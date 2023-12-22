{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Aeson.Examples.Restaurant.Model where

import Protolude

newtype Natural = Natural Integer deriving (Eq, Ord, Show)

tryNatural :: Integer -> Maybe Natural
tryNatural n
  | n < 1 = Nothing
  | otherwise = Just (Natural n)

{-# COMPLETE N #-}
pattern N :: Integer -> Natural
pattern N i <- Natural i

data SingleTable = SingleTable
  { singleCapacity :: Natural
  , minimalReservation :: Natural
  } deriving (Eq, Ord, Show)

data Table = Single SingleTable | Communal Natural deriving (Eq, Show)
