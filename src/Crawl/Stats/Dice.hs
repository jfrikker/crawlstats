{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Crawl.Stats.Dice (
  Probability,
  roll,
  rollScaled,
  divRandRound,
  Dice,
  Normable,
  norm,
  cdt,
  reverseCdt,
  probTable
) where

import Data.Ratio (Rational, (%))
import qualified Numeric.Probability.Distribution as Prob hiding (uniform)
import qualified Numeric.Probability.Object as Prob
import Text.PrettyPrint.Boxes ((//))
import qualified Text.PrettyPrint.Boxes as Boxes
import Text.Printf (printf)
import qualified Data.List as List

type Probability = Rational

type Dist a = Prob.T Probability a

class Normable n where
  norm :: n -> n

class (Monad d, Prob.C Probability d) => Dice d

instance Dice (Prob.T Probability)

instance Ord a => Normable (Prob.T Probability a) where
  norm = Prob.norm

roll :: (Dice m) => Integer -> m Integer
roll m | m <= 1 = return 0
       | otherwise = Prob.uniform [0..(m - 1)]

rollScaled :: (Dice m) => Integer -> Integer -> m Integer
rollScaled m s | m <= 0 = return 0
               | otherwise = let
                 uniOptions = [0 .. (m `div` s) - 1]
                 uniFreqs = map (\a -> (a, 1 % 1)) uniOptions
                 freqs = (m `div` s, m `mod` s % s) : uniFreqs
                 in Prob.fromFrequencies freqs

divRandRound :: (Dice m) => Integer -> Integer -> m Integer
divRandRound num den = Prob.fromFrequencies [(num `div` den, zeroProb), (num `div` den + 1, (1 % 1) - zeroProb)]
  where zeroProb = rem num den % den

space :: Boxes.Box
space = Boxes.char ' '

hGlue :: [Boxes.Box] -> Boxes.Box
hGlue = Boxes.punctuateH Boxes.center1 space

formatPercent :: Probability -> String
formatPercent p = printf "%.0f%%" $ 100 * (fromRational p :: Double)

probTable :: (a -> String) -> [(a, Probability)] -> Boxes.Box
probTable f dist = hGlue $ map col dist
  where col (v, p) = Boxes.text (f v) // Boxes.text (formatPercent p)

cdt :: [(a, Probability)] -> [(a, Probability)]
cdt = snd . List.mapAccumL inc (0 % 1)
  where inc sofar (v, p) = (sofar + p, (v, sofar + p))

reverseCdt :: [(a, Probability)] -> [(a, Probability)]
reverseCdt = reverse . cdt . reverse
