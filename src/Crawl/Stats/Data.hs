module Crawl.Stats.Data (
  Weapon(..),
  CrawlData(..),
  loadData,
  findWeapon,
  findArmour,
  findShield,
  findMonster
) where

import qualified Data.Csv as CSV
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Vector as Vector
import Data.List (find)
import Data.Char (toLower)

import Crawl.Stats.Weapon (Weapon)
import qualified Crawl.Stats.Weapon as Weapon
import Crawl.Stats.Armour (Armour)
import qualified Crawl.Stats.Armour as Armour
import Crawl.Stats.Shield (Shield)
import qualified Crawl.Stats.Shield as Shield
import Crawl.Stats.Monster (Monster)
import qualified Crawl.Stats.Monster as Monster
import Crawl.Stats.Named (Named)
import qualified Crawl.Stats.Named as Named

data CrawlData = CrawlData {
  weapons :: [Weapon],
  armour :: [Armour],
  shields :: [Shield],
  monsters :: [Monster]
}

loadDataFile :: CSV.FromNamedRecord a => FilePath -> String -> IO [a]
loadDataFile dir name = do
  let path = dir ++ "/" ++ name
  bs <- ByteString.readFile path
  let d = CSV.decodeByName bs
  case d of
    Left message -> fail message
    Right (_, d) -> return $ Vector.toList d

loadData :: FilePath -> IO CrawlData
loadData dir = do
  weapons <- loadDataFile dir "weapons.csv"
  armour <- loadDataFile dir "armour.csv"
  shields <- loadDataFile dir "shields.csv"
  monsters <- loadDataFile dir "monsters.csv"
  return CrawlData { weapons = weapons, armour = armour, shields = shields, monsters = monsters }

findWithName :: Named.Named a => String -> [a] -> Maybe a
findWithName = Named.find . map toLower

findWeapon :: String -> CrawlData -> Maybe Weapon
findWeapon name = findWithName name . weapons

findArmour :: String -> CrawlData -> Maybe Armour
findArmour name = findWithName name . armour

findShield :: String -> CrawlData -> Maybe Shield
findShield name = findWithName name . shields

findMonster :: String -> CrawlData -> Maybe Monster
findMonster name = findWithName name . monsters
