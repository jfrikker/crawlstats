{-# LANGUAGE FlexibleContexts #-}

module Crawl.Stats.Attack (
  Attack(..),
  damagePerAttack,
  hpAfter
) where

import Crawl.Stats.Dice
import Crawl.Stats.Player (Player)
import qualified Crawl.Stats.Player as Player
import Crawl.Stats.Monster (Monster)
import qualified Crawl.Stats.Monster as Monster
import qualified Data.List as List
import Control.Monad.Loops (concatM)

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

data Attack = PM Player Monster | MP Monster Player

damagePerAttack :: (Dice m, Normable (m Integer)) => Attack -> m Integer
damagePerAttack (PM player monster) = norm $ do
  hit <- testHit monster player
  fullDam <- if hit
    then Player.meleeDamage player
    else return 0
  applyAc monster player fullDam

weaponSpeed :: Attack -> Integer
weaponSpeed (PM player _) = Player.weaponSpeed player

defenderMaxHp :: (Dice m, Normable (m Integer)) => Attack -> m Integer
defenderMaxHp (PM _ monster) = Monster.hp monster

attack :: (Dice m, Normable (m Integer)) => Attack -> Integer -> m Integer
attack _ 0 = return 0
attack atk hp = do
  d <- damagePerAttack atk
  return $ max 0 $ hp - d

hpAfter :: (Dice m, Normable (m Integer)) => Attack -> [m Integer]
hpAfter atk = List.scanl hpAtTurn (defenderMaxHp atk) [1..]
  where hpAtTurn prev num = norm $ do
                              let numRolls = (10 * num `div` speed) - (10 * (num - 1) `div` speed)
                              p <- prev
                              concatM (replicate (fromIntegral numRolls) (attack atk)) p
        speed = weaponSpeed atk
