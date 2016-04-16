{-# LANGUAGE DeriveGeneric #-}

module Crawl.Stats.Weapon where

import qualified Data.Csv as CSV
import GHC.Generics (Generic)

import qualified Crawl.Stats.Named as Named

data Weapon = Weapon {
  name :: String,
  dam :: Integer,
  hit :: Integer,
  speed :: Integer,
  strWeight :: Integer
} deriving Generic

instance CSV.FromNamedRecord Weapon

instance Named.Named Weapon where
  name = name
