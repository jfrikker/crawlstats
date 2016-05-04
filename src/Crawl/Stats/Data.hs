module Crawl.Stats.Data (
  CrawlData,
  loadData,
  Loaded(..),
) where

import qualified Data.Csv as CSV
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Vector as Vector
import Data.List (find)
import Data.Char (toLower)
import Data.Default (def)

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
  return CrawlData { weapons = def : weapons, 
                     armour = def : armour, 
                     shields = def : shields, 
                     monsters = monsters }

findWithName :: Named.Named a => String -> String -> [a] -> Either String a
findWithName t name = maybe (Left $ "Unknown " ++ t ++ " \"" ++ name ++ "\"") Right . Named.find (map toLower name)

class Named l => Loaded l where
  list :: CrawlData -> [l]
  find :: String -> CrawlData -> Either String l

instance Loaded Weapon where
  list = weapons

  find name = findWithName "weapon" name . list

instance Loaded Armour where
  list = armour

  find name = findWithName "armour" name . list

instance Loaded Shield where
  list = shields

  find name = findWithName "shield" name . list

instance Loaded Monster where
  list = monsters

  find name = findWithName "monster" name . list
