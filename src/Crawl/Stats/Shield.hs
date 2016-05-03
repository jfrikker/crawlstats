{-# LANGUAGE DeriveGeneric #-}

module Crawl.Stats.Shield where

import qualified Data.Csv as CSV
import GHC.Generics (Generic)
import qualified Data.Default as Default

import qualified Crawl.Stats.Named as Named

data Shield = Shield {
  name :: String,
  block :: Integer,
  evPenalty :: Integer,
  dexContrib :: Integer,
  strContrib :: Integer
} deriving (Generic, Eq)

instance CSV.FromNamedRecord Shield

instance Named.Named Shield where
  name = name

instance Default.Default Shield where
  def = Shield "none" 0 0 0 0
