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

import qualified PlayerSer

data TempKV = TempKV

instance PlayerSer.KVStore TempKV where
 getString "hp" _ = Right "48"
 getString "ev" _ = Right "6"
 getString "str" _ = Right "20"
 getString "int" _ = Right "1"
 getString "dex" _ = Right "9"
 getString "weapon" _ = Right "mace"
 getString "armour" _ = Right "plate"
 getString "shield" _ = Right "shield"
 getString "fightingSkill" _ = Right "3"
 getString "macesSkill" _ = Right "3"
 getString "armourSkill" _ = Right "10"
 getString "shieldSkill" _ = Right "0"
 getString o _ = Left $ "Unknown key " ++ o

deadBeforeTable :: [T Dice.Probability Integer] -> Boxes.Box
deadBeforeTable autProbs = Dice.probTable (show . (`div` 10)) turnProbs
  where zeroProbs = map ((== 0) ??) autProbs
        topN = takeWhile (< (99/100)) zeroProbs
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
  putStrLn "Block chance:"
  putStrLn $ Dice.formatPercent $ id ?? Attack.block atk
  putStrLn "Real damage / attack:"
  Boxes.printBox $ Dice.probTable show $ Dice.reverseCdt $ decons $ Attack.damagePerAttack atk
  putStrLn "Weapon Speed:"
  Boxes.printBox $ Dice.probTable id $ decons $ toAut $ Attack.weaponSpeed atk
  putStrLn "Dead after:"
  Boxes.printBox $ deadBeforeTable $ Attack.hpAfter atk

main :: IO ()
main = do
  cd <- CrawlData.loadData "data"
  p <- either fail return $ PlayerSer.loadPlayer cd TempKV

  let Right monster = CrawlData.findMonster "bat" cd
  print $ Player.block p

  putStrLn ("Player -> " ++ name monster)
  printAttack $ Attack.PM p monster

  putStrLn (name monster ++ " -> Player")
  printAttack $ Attack.MP monster p
