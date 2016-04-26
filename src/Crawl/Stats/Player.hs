{-# LANGUAGE FlexibleContexts #-}

module Crawl.Stats.Player (
  Player(..),
  toHit,
  meleeDamage,
  adjustedBodyArmourPenalty,
  weaponSpeed,
  ac,
  gdr,
  block
) where

import Crawl.Stats.Dice
import qualified Crawl.Stats.Stats as Stats
import Crawl.Stats.Weapon (Weapon)
import qualified Crawl.Stats.Weapon as Weapon
import Crawl.Stats.Armour (Armour)
import qualified Crawl.Stats.Armour as Armour
import Crawl.Stats.Shield (Shield)
import qualified Crawl.Stats.Shield as Shield
import Control.Monad.State (evalStateT, get, put, StateT, lift)

data Player = Player {
  hp :: Integer,
  ev :: Integer,
  str :: Integer,
  int :: Integer,
  dex :: Integer,
  weapon :: Weapon,
  armour :: Armour,
  shield :: Shield,
  fighting :: Integer,
  macesSkill :: Integer,
  armourSkill :: Integer,
  shieldSkill :: Integer
}

skill :: Stats.Skill -> Player -> Integer
skill Stats.MACES_FLAILS = macesSkill

adjustedBodyArmourPenalty :: Integer -> Player -> Integer
adjustedBodyArmourPenalty scale player = 2 * encumbrance * encumbrance * (450 - armourSkill player * 10)
                                          * scale `div` (5 * (str player + 3)) `div` 450
  where encumbrance = (Armour.encumbrance.armour) player

adjustedShieldPenalty :: Integer -> Player -> Integer
adjustedShieldPenalty scale player = max 0 $ (basePenalty * scale - shieldSkill player * scale `div` 5 * 10) `div` 10
  where basePenalty = (Shield.evPenalty.shield) player

armourToHitPenalty :: Dice m => Player -> m Integer
armourToHitPenalty player = rollScaled (adjustedBodyArmourPenalty 20 player) 20

shieldToHitPenalty :: Dice m => Player -> m Integer
shieldToHitPenalty player = rollScaled (adjustedShieldPenalty 20 player) 20

calcStatToHitBase :: Player -> Integer
calcStatToHitBase player = dex player + (str player - dex player) * (Weapon.strWeight.weapon) player `div` 20

weaponSkill :: Player -> Integer
weaponSkill player = skill (Weapon.skill $ weapon player) player

toHit :: (Dice m, Normable (m Integer)) => Player -> m Integer
toHit player = norm $ do
  fromFighting <- roll $ fighting player
  fromWeaponSkill <- roll $ weaponSkill player
  bap <- armourToHitPenalty player
  shieldPenalty <- shieldToHitPenalty player
  let fromWeapon = Weapon.hit $ weapon player
  let max = 15 +
            (calcStatToHitBase player `div` 2) +
            fromFighting +
            fromWeaponSkill +
            fromWeapon -
            bap -
            shieldPenalty
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

weaponSpeed :: (Dice m, Normable (m Integer)) => Player -> m Integer
weaponSpeed player = norm $ do
  let baseSpeed = Weapon.speed $ weapon player
  let minDelay = min 7 $ baseSpeed `div` 2
  let baseShieldPenalty = adjustedShieldPenalty 20 player
  skillAdj <- divRandRound (weaponSkill player * 10) 20
  shieldAdj <- if baseShieldPenalty == 0
                 then return 0
                 else do
                   r <- minRoll 2 baseShieldPenalty
                   divRandRound r 20
  return $ max minDelay $ baseSpeed - skillAdj + shieldAdj

ac :: Player -> Integer
ac player = (base + fromSkill) `div` 100
  where base = 100 * Armour.baseAc (armour player)
        fromSkill = base * armourSkill player `div` 22

gdr :: Player -> Integer
gdr = Armour.gdr . armour

block :: Player -> Integer
block player
  | baseShield == 0 = 0
  | otherwise = (50 + fromShield + extraFromSkill + fromSkill + fromStats) `div` 100
  where baseShield = Shield.block (shield player) * 2
        fromShield = baseShield * 50
        extraFromSkill = baseShield * shieldSkill player * 5 `div` 2
        fromSkill = shieldSkill player * 38 + min (3 * 38) (shieldSkill player * 38)
        fromDex = Shield.dexContrib (shield player) * dex player
        fromStr = Shield.strContrib (shield player) * str player
        fromStats = (fromDex + fromStr) * (baseShield + 13) `div` 26
