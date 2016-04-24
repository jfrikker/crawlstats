{-# LANGUAGE FlexibleContexts #-}

module Crawl.Stats.Attack (
  Attack(..),
  toHit,
  damage,
  damagePerAttack,
  weaponSpeed,
  defenderMaxHp,
  evasion,
  ac,
  testHit,
  hpAfter
) where

import Crawl.Stats.Dice
import Crawl.Stats.Player (Player)
import qualified Crawl.Stats.Player as Player
import Crawl.Stats.Monster (Monster)
import qualified Crawl.Stats.Monster as Monster
import qualified Data.List as List
import qualified Data.Array as Array
import Data.Array ((!))
import Debug.Trace

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

ac :: Attack -> Integer
ac (PM _ monster) = Monster.ac monster
ac (MP _ player) = Player.ac player

gdr :: Attack -> Integer
gdr (PM _ monster) = Monster.gdr monster
gdr (MP _ player) = Player.gdr player

applyAc :: (Dice m) => Attack -> Integer -> m Integer
applyAc atk damage = do
  fromAc <- roll (1 + ac atk)
  let fromGdr = min (gdr atk * damage `div` 100) (ac atk `div` 2)
  let saved = max fromAc fromGdr
  return $ max 0 $ damage - saved

data Attack = PM Player Monster | MP Monster Player

toHit :: (Dice m, Normable (m Integer)) => Attack -> m Integer
toHit (PM player _) = Player.toHit player
toHit (MP monster _) = return $ Monster.toHit monster

damage :: (Dice m, Normable (m Integer)) => Attack -> m Integer
damage (PM player _) = Player.meleeDamage player
damage (MP monster _) = do
  r <- roll $ Monster.attack monster
  return $ r + 1

damagePerAttack :: (Dice m, Normable (m Integer)) => Attack -> m Integer
damagePerAttack atk = norm $ do
  hit <- testHit atk
  fullDam <- if hit
    then damage atk
    else return 0
  applyAc atk fullDam

weaponSpeed :: (Dice m, Normable (m Integer)) => Attack -> m Integer
weaponSpeed (PM player _) = Player.weaponSpeed player
weaponSpeed (MP monster _) = return 10

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
hpAfter atk = Array.elems memo
  where maxHp = defenderMaxHp atk
        speed = weaponSpeed atk
        memo = Array.listArray (0, 500) $ map hpAtAut [0..500]
        hpAtAut aut = norm $ do
          delay <- speed
          let last = aut - delay
          if last < 0
            then maxHp
            else do
              hp <- memo ! last
              attack atk hp

