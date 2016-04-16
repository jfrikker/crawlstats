module Main where

import qualified Crawl.Stats.Dice as Dice
import Crawl.Stats.Player (Player(..))
import qualified Crawl.Stats.Player as Player 
import Crawl.Stats.Data (CrawlData)
import qualified Crawl.Stats.Data as CrawlData

import Numeric.Probability.Distribution
import qualified Text.PrettyPrint.Boxes as Boxes
import Data.Maybe (fromJust)
import Data.Ratio ((%))

player cd = Player {
  str = 20,
  int = 1,
  dex = 9,
  weapon = fromJust $ CrawlData.findWeapon "whip" cd,
  armour = fromJust $ CrawlData.findArmour "plate" cd,
  fighting = 3,
  macesSkill = 3,
  armourSkill = 3
}

main :: IO ()
main = do
  cd <- CrawlData.loadData "data"
  let p = player cd
  let toHit = norm $ Player.toHit p
  let toHitList = decons toHit
  Boxes.printBox $ Dice.probTable show $ Dice.reverseCdt toHitList
  let toHitRatio = fmap (% 1) toHit
  print $ fromRational $ expected toHitRatio
  print $ sqrt $ fromRational $ variance toHitRatio

  let damage = norm $ Player.meleeDamage p
  Boxes.printBox $ Dice.probTable show $ Dice.reverseCdt $ decons $ norm damage
