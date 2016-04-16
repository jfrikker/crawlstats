{-# LANGUAGE DeriveGeneric #-}

module Crawl.Stats.Armour where

import qualified Data.Csv as CSV
import GHC.Generics (Generic)

import qualified Crawl.Stats.Named as Named

data Armour = Armour {
  name :: String,
  baseAc :: Integer,
  encumbrance :: Integer,
  gdr :: Integer
} deriving Generic

instance CSV.FromNamedRecord Armour

instance Named.Named Armour where
  name = name
