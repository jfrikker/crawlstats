module Crawl.Stats.Named (
  Named(..),
  hasName,
  find
) where

import qualified Data.List as List

class Named n where
  name :: n -> String

hasName :: Named a => String -> a -> Bool
hasName n a = n == name a

find :: Named a => String -> [a] -> Maybe a
find = List.find . hasName
