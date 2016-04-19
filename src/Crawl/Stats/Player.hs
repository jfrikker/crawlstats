{-# LANGUAGE FlexibleContexts #-}

module Crawl.Stats.Player (
  Player(..),
  toHit,
  meleeDamage,
  adjustedBodyArmourPenalty,
  weaponSpeed
) where

import Crawl.Stats.Dice
import Crawl.Stats.Weapon (Weapon)
import qualified Crawl.Stats.Stats as Stats
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
  macesSkill :: Integer,
  armourSkill :: Integer
}

skill :: Stats.Skill -> Player -> Integer
skill Stats.MACES_FLAILS = macesSkill

adjustedBodyArmourPenalty :: Integer -> Player -> Integer
adjustedBodyArmourPenalty scale player = 2 * encumbrance * encumbrance * (450 - armourSkill player)
                                          * scale `div` (5 * (str player + 3)) `div` 450
  where encumbrance = (Armour.encumbrance.armour) player

armourToHitPenalty :: Dice m => Player -> m Integer
armourToHitPenalty player = roll $ adjustedBodyArmourPenalty 20 player

calcStatToHitBase :: Player -> Integer
calcStatToHitBase player = dex player + (str player - dex player) * (Weapon.strWeight.weapon) player `div` 20

weaponSkill :: Player -> Integer
weaponSkill player = skill (Weapon.skill $ weapon player) player

toHit :: (Dice m, Normable (m Integer)) => Player -> m Integer
toHit player = norm $ do
  fromFighting <- roll $ fighting player
  fromWeaponSkill <- roll $ weaponSkill player
  bap <- armourToHitPenalty player
  randbap <- divRandRound bap 20
  let fromWeapon = Weapon.hit $ weapon player
  let max = 15 +
            (calcStatToHitBase player `div` 2) +
            fromFighting +
            fromWeaponSkill +
            fromWeapon -
            randbap
  roll max

calcStatToDamBase :: Player -> Integer
calcStatToDamBase player = str player + (dex player - str player) * (10 - (Weapon.strWeight.weapon) player) `div` 20

playerStatModifyDamage :: Dice m => Player -> Integer -> m Integer
playerStatModifyDamage player damage
  | damStatVal > 11 = do
    extra <- roll $ damStatVal - 11
    return $ damage * (39 + extra * 2) `div` 39
  | damStatVal < 9 = do
    extra <- roll $ 9 - damStatVal
    return $ damage * (39 - extra * 3) `div` 39
  | otherwise = return damStatVal
  where damStatVal = calcStatToDamBase player

playerApplyFightingSkill :: Dice m => Player -> Integer -> m Integer
playerApplyFightingSkill player damage = do
  let num = (fighting player * 100) + 1
  r <- rollScaled (num * damage) 4000
  return $ r + damage

playerApplyWeaponSkill :: Dice m => Player -> Integer -> m Integer
playerApplyWeaponSkill player damage = do
  let num = (weaponSkill player * 100) + 1
  r <- rollScaled (num * damage) 2500
  return $ r + damage

modifyM :: Monad m => (s -> m s) -> StateT s m ()
modifyM m = do
  current <- get
  temp <- lift $ m current
  put temp

getsM :: Monad m => (s -> m a) -> StateT s m a
getsM f = do
  s <- get
  lift $ f s

meleeDamage :: (Dice m, Normable (m Integer)) => Player -> m Integer
meleeDamage player = norm $ evalStateT (do
  put $ (Weapon.dam.weapon) player
  modifyM $ playerStatModifyDamage player
  modifyM $ roll . (+ 1)
  modifyM $ playerApplyWeaponSkill player
  modifyM $ playerApplyFightingSkill player
  get) 0

weaponSpeed :: Player -> Integer
weaponSpeed player = max minDelay $ baseSpeed - skillAdj
  where baseSpeed = Weapon.speed $ weapon player
        minDelay = min 7 $ baseSpeed `div` 2
        skillAdj = weaponSkill player `div` 2
