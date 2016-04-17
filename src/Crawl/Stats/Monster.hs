{-# LANGUAGE DeriveGeneric #-}

module Crawl.Stats.Monster where

import qualified Data.Csv as CSV
import GHC.Generics (Generic)

import qualified Crawl.Stats.Named as Named
import Crawl.Stats.Stats (Skill)

data Monster = Monster {
  name :: String,
  ev :: Integer,
  ac :: Integer
} deriving Generic

instance CSV.FromNamedRecord Monster

instance Named.Named Monster where
  name = name
