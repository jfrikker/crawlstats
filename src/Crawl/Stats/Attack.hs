{-# LANGUAGE FlexibleContexts #-}

module Crawl.Stats.Attack (
  Attack(..),
  toHit,
  damage,
  damagePerAttack,
  weaponSpeed,
  defenderMaxHp,
  evasion,
  testHit,
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

testHit :: (Dice m, Normable (m Integer)) => Attack -> m Bool
testHit atk = do
  automatic <- xChanceInY minHitMissPercentage 100
  if automatic
    then xChanceInY 1 2
    else do
      tohit <- toHit atk
      ev <- evasion atk
      return $ tohit >= ev

applyAc :: (Dice m) => Monster -> Player -> Integer -> m Integer
applyAc monster player damage = do
  saved <- roll (1 + Monster.ac monster)
  return $ max (damage - saved) 0

data Attack = PM Player Monster | MP Monster Player

toHit :: (Dice m, Normable (m Integer)) => Attack -> m Integer
toHit (PM player _) = Player.toHit player
toHit (MP monster _) = return $ Monster.toHit monster

damage :: (Dice m, Normable (m Integer)) => Attack -> m Integer
damage (PM player _) = Player.meleeDamage player
damage (MP monster _) = return $ Monster.attack monster

damagePerAttack :: (Dice m, Normable (m Integer)) => Attack -> m Integer
damagePerAttack atk@(PM player monster) = norm $ do
  hit <- testHit atk
  fullDam <- if hit
    then damage atk
    else return 0
  applyAc monster player fullDam

weaponSpeed :: Attack -> Integer
weaponSpeed (PM player _) = Player.weaponSpeed player
weaponSpeed (MP monster _) = 10

defenderMaxHp :: (Dice m, Normable (m Integer)) => Attack -> m Integer
defenderMaxHp (PM _ monster) = Monster.hp monster
defenderMaxHp (MP _ player) = return $ Player.hp player

evasion :: (Dice m, Normable (m Integer)) => Attack -> m Integer
evasion (PM _ monster) = return $ Monster.ev monster
evasion (MP _ player) = norm $ rollAveraged 2 $ 2 * Player.ev player

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
