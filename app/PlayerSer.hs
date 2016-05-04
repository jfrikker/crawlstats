module PlayerSer where

import Crawl.Stats.Player (Player(..))
import Crawl.Stats.Data (CrawlData)
import qualified Crawl.Stats.Data as CrawlData
import Text.Read (readEither)

class KVStore k where
 getString :: String -> k -> Either String String
 getValue :: Read v => String -> k -> Either String v
 getValue key kv = do
  s <- getString key kv
  readEither s

loadPlayer :: KVStore k => CrawlData -> k -> Either String Player
loadPlayer cd kv = do
  hp <- getValue "hp" kv
  ev <- getValue "ev" kv
  str <- getValue "str" kv
  int <- getValue "int" kv
  dex <- getValue "dex" kv
  weaponName <- getString "weapon" kv
  weapon <- CrawlData.find weaponName cd
  armourName <- getString "armour" kv
  armour <- CrawlData.find armourName cd
  shieldName <- getString "shield" kv
  shield <- CrawlData.find shieldName cd
  fightingSkill <- getValue "fightingSkill" kv
  macesSkill <- getValue "macesSkill" kv
  armourSkill <- getValue "armourSkill" kv
  shieldSkill <- getValue "shieldSkill" kv
  return Player {hp = hp, ev = ev, str = str, int = int, dex = dex,
                 weapon = weapon, armour = armour, shield = shield,
                 fightingSkill = fightingSkill, macesSkill = macesSkill,
                 armourSkill = armourSkill, shieldSkill = shieldSkill}
