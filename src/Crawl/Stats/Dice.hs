{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Crawl.Stats.Dice (
  Probability,
  Prob.fromFrequencies,
  roll,
  rollScaled,
  rollAveraged,
  divRandRound,
  minRoll,
  xChanceInY,
  Dice,
  Normable,
  norm,
  cdt,
  reverseCdt,
  probTable,
  formatPercent
) where

import qualified Numeric.Probability.Distribution as Prob hiding (uniform)
import qualified Numeric.Probability.Object as Prob
import Text.PrettyPrint.Boxes ((//))
import qualified Text.PrettyPrint.Boxes as Boxes
import Text.Printf (printf)
import qualified Data.List as List
import Control.Monad.Loops (concatM)

type Probability = Double

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
rollScaled m s | m < s = return 0
               | otherwise = let
                 uniOptions = [0 .. (m `div` s) - 1]
                 uniFreqs = map (\a -> (a, fromIntegral s)) uniOptions
                 freqs = (m `div` s, fromIntegral $ m `mod` s) : uniFreqs
                 in Prob.fromFrequencies freqs

rollAveraged :: (Dice m) => Integer -> Integer -> m Integer
rollAveraged num max = do
  tot <- concatM (replicate (fromIntegral num) doRoll) 0
  return $ tot `div` num
  where doRoll tot = do
                       r <- roll max
                       return $ r + tot

divRandRound :: (Dice m) => Integer -> Integer -> m Integer
divRandRound num den = Prob.fromFrequencies [(num `div` den, zeroProb), (num `div` den + 1, 1 - zeroProb)]
  where zeroProb = fromIntegral (rem num den) / fromIntegral den

minRoll :: (Dice m) => Integer -> Integer -> m Integer
minRoll num max = Prob.fromFrequencies $ map elem [1 .. max]
  where weight i = fromIntegral $ (max - i + 1) ^ num - (max - i) ^ num
        elem i = (i - 1, weight i)

xChanceInY :: (Dice m) => Integer -> Integer -> m Bool
xChanceInY x y = Prob.fromFrequencies [(True, fromIntegral x), (False, fromIntegral $ y - x)]

space :: Boxes.Box
space = Boxes.char ' '

hGlue :: [Boxes.Box] -> Boxes.Box
hGlue = Boxes.punctuateH Boxes.center1 space

formatPercent :: Probability -> String
formatPercent p = printf "%.0f%%" $ 100 * p

probTable :: (a -> String) -> [(a, Probability)] -> Boxes.Box
probTable f dist = hGlue $ map col dist
  where col (v, p) = Boxes.text (f v) // Boxes.text (formatPercent p)

cdt :: [(a, Probability)] -> [(a, Probability)]
cdt = snd . List.mapAccumL inc 0
  where inc sofar (v, p) = (sofar + p, (v, sofar + p))

reverseCdt :: [(a, Probability)] -> [(a, Probability)]
reverseCdt = reverse . cdt . reverse
