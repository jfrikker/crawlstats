{-# LANGUAGE DeriveGeneric #-}

module Crawl.Stats.Monster where

import qualified Data.Csv as CSV
import GHC.Generics (Generic)

import qualified Crawl.Stats.Named as Named
import Crawl.Stats.Stats (Skill)
import Crawl.Stats.Dice (Dice, roll)

data Monster = Monster {
  name :: String,
  ev :: Integer,
  ac :: Integer,
  minHp :: Integer,
  maxHp :: Integer,
  hd :: Integer
} deriving Generic

instance CSV.FromNamedRecord Monster

instance Named.Named Monster where
  name = name

hp :: Dice m => Monster -> m Integer
hp monster = do
  r <- roll (maxHp monster - minHp monster)
  return $ minHp monster + r

toHit :: Monster -> Integer
toHit monster = 18 + hd monster * 15 `div` 10
