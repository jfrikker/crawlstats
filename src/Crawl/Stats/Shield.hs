{-# LANGUAGE DeriveGeneric #-}

module Crawl.Stats.Shield where

import qualified Data.Csv as CSV
import GHC.Generics (Generic)

import qualified Crawl.Stats.Named as Named

data Shield = Shield {
  name :: String,
  evPenalty :: Integer
} deriving Generic

instance CSV.FromNamedRecord Shield

instance Named.Named Shield where
  name = name
