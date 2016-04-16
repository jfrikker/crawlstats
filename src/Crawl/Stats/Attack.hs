{-# LANGUAGE FlexibleContexts #-}

module Crawl.Stats.Attack (
  playerDamage
) where

import Crawl.Stats.Dice
import Crawl.Stats.Player (Player)
import qualified Crawl.Stats.Player as Player
import Crawl.Stats.Monster (Monster)
import qualified Crawl.Stats.Monster as Monster

playerDamage :: (Dice m, Normable (m Integer)) => Monster -> Player -> m Integer
playerDamage monster player = norm $ do
  tohit <- Player.toHit player
  let hit = tohit >= Monster.ev monster
  if hit
    then Player.meleeDamage player
    else return 0
