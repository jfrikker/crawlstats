module Crawl.Stats.Lens.Player (
  Player,
  hp,
  ev,
  str,
  int,
  dex,
  weapon,
  armour,
  shield,
  fightingSkill,
  macesSkill,
  armourSkill,
  shieldSkill
) where

import Crawl.Stats.Player (Player(Player))
import qualified Crawl.Stats.Player as Player
import Crawl.Stats.Weapon (Weapon)
import Crawl.Stats.Armour (Armour)
import Crawl.Stats.Shield (Shield)
import Control.Lens

hp :: Lens' Player Integer
hp f p@Player {Player.hp = a} = (\a' -> p {Player.hp = a'}) <$> f a

ev :: Lens' Player Integer
ev f p@Player {Player.ev = a} = (\a' -> p {Player.ev = a'}) <$> f a

str :: Lens' Player Integer
str f p@Player {Player.str = a} = (\a' -> p {Player.str = a'}) <$> f a

int :: Lens' Player Integer
int f p@Player {Player.int = a} = (\a' -> p {Player.int = a'}) <$> f a

dex :: Lens' Player Integer
dex f p@Player {Player.dex = a} = (\a' -> p {Player.dex = a'}) <$> f a

weapon :: Lens' Player Weapon
weapon f p@Player {Player.weapon = a} = (\a' -> p {Player.weapon = a'}) <$> f a

armour :: Lens' Player Armour
armour f p@Player {Player.armour = a} = (\a' -> p {Player.armour = a'}) <$> f a

shield :: Lens' Player Shield
shield f p@Player {Player.shield = a} = (\a' -> p {Player.shield = a'}) <$> f a

fightingSkill :: Lens' Player Integer
fightingSkill f p@Player {Player.fightingSkill = a} = (\a' -> p {Player.fightingSkill = a'}) <$> f a

macesSkill :: Lens' Player Integer
macesSkill f p@Player {Player.macesSkill = a} = (\a' -> p {Player.macesSkill = a'}) <$> f a

armourSkill :: Lens' Player Integer
armourSkill f p@Player {Player.armourSkill = a} = (\a' -> p {Player.armourSkill = a'}) <$> f a

shieldSkill :: Lens' Player Integer
shieldSkill f p@Player {Player.shieldSkill = a} = (\a' -> p {Player.shieldSkill = a'}) <$> f a
