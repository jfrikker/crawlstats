{-# LANGUAGE DeriveGeneric #-}

module Crawl.Stats.Armour where

import qualified Data.Csv as CSV
import GHC.Generics (Generic)
import qualified Data.Default as Default

import qualified Crawl.Stats.Named as Named

data Armour = Armour {
  name :: String,
  baseAc :: Integer,
  encumbrance :: Integer,
  gdr :: Integer
} deriving (Generic, Eq)

instance CSV.FromNamedRecord Armour

instance Named.Named Armour where
  name = name

instance Default.Default Armour where
  def = Armour "none" 0 0 0
