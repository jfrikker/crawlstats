{-# LANGUAGE FlexibleContexts #-}

module Crawl.Stats.Player (
  Player(..),
  toHit,
  meleeDamage,
  adjustedBodyArmourPenalty
) where

import Crawl.Stats.Dice
import Crawl.Stats.Weapon (Weapon)
import qualified Crawl.Stats.Weapon as Weapon
import Crawl.Stats.Armour (Armour)
import qualified Crawl.Stats.Armour as Armour
import Control.Monad.State (evalStateT, get, put, StateT, lift)

data Player = Player {
  str :: Integer,
  int :: Integer,
  dex :: Integer,
  weapon :: Weapon,
  armour :: Armour,
  fighting :: Integer,
  maces :: Integer,
  armourSk :: Integer
}

adjustedBodyArmourPenalty :: Integer -> Player -> Integer
adjustedBodyArmourPenalty scale player = 2 * encumbrance * encumbrance * (450 - armourSk player)
                                          * scale `div` (5 * (str player + 3)) `div` 450
  where encumbrance = (Armour.encumbrance.armour) player

armourToHitPenalty :: (Monad m, C Probability m) => Player -> m Integer
armourToHitPenalty player = roll $ adjustedBodyArmourPenalty 20 player

calcStatToHitBase :: Player -> Integer
calcStatToHitBase player = dex player + (str player - dex player) * (Weapon.strWeight.weapon) player `div` 20

toHit :: (Monad m, C Probability m) => Player -> m Integer
toHit player = do
  fromFighting <- roll $ fighting player
  fromWeaponSkill <- roll $ maces player
  bap <- armourToHitPenalty player
  randbap <- divRandRound bap 20
  let max = 15 +
            calcStatToHitBase player +
            fromFighting +
            fromWeaponSkill -
            randbap
  roll max

calcStatToDamBase :: Player -> Integer
calcStatToDamBase player = str player + (dex player - str player) * (10 - (Weapon.strWeight.weapon) player) `div` 20

playerStatModifyDamage :: (Monad m, C Probability m) => Player -> Integer -> m Integer
playerStatModifyDamage player damage
  | damStatVal > 11 = do
    extra <- roll $ damStatVal - 11
    return $ damage * (39 + extra * 2) `div` 39
  | damStatVal < 9 = do
    extra <- roll $ 9 - damStatVal
    return $ damage * (39 - extra * 3) `div` 39
  | otherwise = return damStatVal
  where damStatVal = calcStatToDamBase player

playerApplyFightingSkill :: (Monad m, C Probability m) => Player -> Integer -> m Integer
playerApplyFightingSkill player damage = do
  r <- roll $ (fighting player * 100) + 1
  return $ damage * (2500 + r) `div` 2500

modifyM :: Monad m => (s -> m s) -> StateT s m ()
modifyM m = do
  current <- get
  temp <- lift $ m current
  put temp

getsM :: Monad m => (s -> m a) -> StateT s m a
getsM f = do
  s <- get
  lift $ f s

meleeDamage :: (Monad m, C Probability m) => Player -> m Integer
meleeDamage player = evalStateT (do
  put $ (Weapon.dam.weapon) player
  modifyM $ playerStatModifyDamage player
  modifyM $ playerApplyFightingSkill player
  getsM roll) 0
