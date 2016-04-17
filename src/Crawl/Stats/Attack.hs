{-# LANGUAGE FlexibleContexts #-}

module Crawl.Stats.Attack (
  playerDamage
) where

import Crawl.Stats.Dice
import Crawl.Stats.Player (Player)
import qualified Crawl.Stats.Player as Player
import Crawl.Stats.Monster (Monster)
import qualified Crawl.Stats.Monster as Monster

minHitMissPercentage :: Integer
minHitMissPercentage = 5

testHit :: (Dice m, Normable (m Integer)) => Monster -> Player -> m Bool
testHit monster player = do
  automatic <- xChanceInY minHitMissPercentage 100
  if automatic
    then xChanceInY 1 2
    else do
      tohit <- Player.toHit player
      return $ tohit >= Monster.ev monster

applyAc :: (Dice m) => Monster -> Player -> Integer -> m Integer
applyAc monster player damage = do
  saved <- roll (1 + Monster.ac monster)
  return $ max (damage - saved) 0

playerDamage :: (Dice m, Normable (m Integer)) => Monster -> Player -> m Integer
playerDamage monster player = norm $ do
  hit <- testHit monster player
  fullDam <- if hit
    then Player.meleeDamage player
    else return 0
  applyAc monster player fullDam
