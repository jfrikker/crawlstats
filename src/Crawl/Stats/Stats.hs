module Crawl.Stats.Stats (
  Skill(..)
)where

import qualified Data.Csv as CSV
import Control.Monad.Plus (mfromMaybe)
import qualified Data.List as List
import Data.ByteString.Char8 (unpack)
import Text.Read (readMaybe)

data Skill = MACES_FLAILS deriving (Eq, Ord, Enum, Show, Bounded, Read)

parseRead :: (Read f) => CSV.Field -> CSV.Parser f
parseRead = mfromMaybe . readMaybe . unpack

instance CSV.FromField Skill where
  parseField = parseRead
