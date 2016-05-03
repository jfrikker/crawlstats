{-# LANGUAGE DeriveGeneric #-}

module Crawl.Stats.Weapon where

import qualified Data.Csv as CSV
import GHC.Generics (Generic)
import qualified Data.Default as Default

import qualified Crawl.Stats.Named as Named
import Crawl.Stats.Stats (Skill)
import qualified Crawl.Stats.Stats as Stats

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

-- TODO
instance Default.Default Weapon where
  def = Weapon "unarmed" 0 0 10 50 Stats.MACES_FLAILS
