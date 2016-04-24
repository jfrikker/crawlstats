module Main where

import qualified Crawl.Stats.Dice as Dice
import Crawl.Stats.Player (Player(..))
import qualified Crawl.Stats.Player as Player 
import Crawl.Stats.Data (CrawlData)
import qualified Crawl.Stats.Data as CrawlData
import Crawl.Stats.Attack (Attack)
import qualified Crawl.Stats.Attack as Attack
import Crawl.Stats.Named (name)

import Numeric.Probability.Distribution hiding (map, filter)
import qualified Text.PrettyPrint.Boxes as Boxes
import Data.Maybe (fromJust)
import Text.Printf (printf)

player cd = Player {
  hp = 48,
  ev = 6,
  str = 20,
  int = 1,
  dex = 9,
  weapon = fromJust $ CrawlData.findWeapon "flail" cd,
  armour = fromJust $ CrawlData.findArmour "plate" cd,
  shield = fromJust $ CrawlData.findShield "shield" cd,
  fighting = 3,
  macesSkill = 3,
  armourSkill = 3,
  shieldSkill = 0
}

deadBeforeTable :: [T Dice.Probability Integer] -> Boxes.Box
deadBeforeTable autProbs = Dice.probTable (show . (`div` 10)) turnProbs
  where zeroProbs = map ((== 0) ??) autProbs
        topN = take 500 $ takeWhile (< (99/100)) zeroProbs
        zipped = zip [0..] topN
        turnProbs = filter (\x -> (fst x `mod` 10) == 0) zipped

toAut :: Num a => T a Integer -> T a String
toAut = norm . fmap format
  where format i = printf "%.1f" ((fromIntegral i :: Double) / 10)

printAttack :: Attack -> IO ()
printAttack atk = do
  putStrLn "HP:"
  Boxes.printBox $ Dice.probTable show $ decons $ Attack.defenderMaxHp atk
  putStrLn "Evasion:"
  Boxes.printBox $ Dice.probTable show $ Dice.reverseCdt $ decons $ Attack.evasion atk
  putStrLn "To Hit:"
  Boxes.printBox $ Dice.probTable show $ Dice.reverseCdt $ decons $ Attack.toHit atk
  putStrLn "Chance to hit:"
  putStrLn $ Dice.formatPercent ((== True) ?? Attack.testHit atk)
  putStrLn "Damage:"
  Boxes.printBox $ Dice.probTable show $ Dice.reverseCdt $ decons $ Attack.damage atk
  putStrLn "AC:"
  print $ Attack.ac atk
  putStrLn "Real damage / attack:"
  Boxes.printBox $ Dice.probTable show $ Dice.reverseCdt $ decons $ Attack.damagePerAttack atk
  putStrLn "Weapon Speed:"
  Boxes.printBox $ Dice.probTable id $ decons $ toAut $ Attack.weaponSpeed atk
  putStrLn "Dead after:"
  Boxes.printBox $ deadBeforeTable $ Attack.hpAfter atk

main :: IO ()
main = do
  Boxes.printBox $ Dice.probTable show $ decons $ Dice.minRoll 100 20
  cd <- CrawlData.loadData "data"
  let p = player cd

  let monster = fromJust $ CrawlData.findMonster "dire_elephant" cd

  putStrLn ("Player -> " ++ name monster)
  printAttack $ Attack.PM p monster

  putStrLn (name monster ++ " -> Player")
  printAttack $ Attack.MP monster p
