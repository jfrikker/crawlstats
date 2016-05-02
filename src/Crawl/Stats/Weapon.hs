{-# LANGUAGE DeriveGeneric #-}

module Crawl.Stats.Weapon where

import qualified Data.Csv as CSV
import GHC.Generics (Generic)

import qualified Crawl.Stats.Named as Named
import Crawl.Stats.Stats (Skill)

data Weapon = Weapon {
  name :: String,
  dam :: Integer,
  hit :: Integer,
  speed :: Integer,
  strWeight :: Integer,
  skill :: Skill
} deriving (Generic, Eq)

instance CSV.FromNamedRecord Weapon

instance Named.Named Weapon where
  name = name
