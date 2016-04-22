module Main where

import qualified Crawl.Stats.Dice as Dice
import Crawl.Stats.Player (Player(..))
import qualified Crawl.Stats.Player as Player 
import Crawl.Stats.Data (CrawlData)
import qualified Crawl.Stats.Data as CrawlData
import qualified Crawl.Stats.Attack as Attack

import Numeric.Probability.Distribution hiding (map)
import qualified Text.PrettyPrint.Boxes as Boxes
import Data.Maybe (fromJust)
import Data.Ratio ((%))

player cd = Player {
  str = 20,
  int = 1,
  dex = 9,
  weapon = fromJust $ CrawlData.findWeapon "whip" cd,
  armour = fromJust $ CrawlData.findArmour "plate" cd,
  shield = fromJust $ CrawlData.findShield "shield" cd,
  fighting = 3,
  macesSkill = 3,
  armourSkill = 3,
  shieldSkill = 0
}

deadBeforeTable :: [T Dice.Probability Integer] -> Boxes.Box
deadBeforeTable turnProbs = Dice.probTable show zipped
  where zeroProb = map ((== 0) ??) turnProbs
        topN = takeWhile (< (99%100)) zeroProb
        zipped = zip [0..] topN

main :: IO ()
main = do
  cd <- CrawlData.loadData "data"
  let p = player cd
  let toHit = Player.toHit p
  let toHitList = decons toHit
  Boxes.printBox $ Dice.probTable show $ Dice.reverseCdt toHitList
  print $ Player.weaponSpeed p

  let damage = Player.meleeDamage p
  Boxes.printBox $ Dice.probTable show $ Dice.reverseCdt $ decons damage

  let monster = fromJust $ CrawlData.findMonster "bat" cd
  let playerDamage = Attack.playerDamage monster p
  Boxes.printBox $ Dice.probTable show $ Dice.reverseCdt $ decons playerDamage

  let monsterHpAfter = Attack.hpAfter p monster
  Boxes.printBox $ deadBeforeTable monsterHpAfter
