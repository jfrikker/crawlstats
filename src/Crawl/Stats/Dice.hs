{-# LANGUAGE FlexibleContexts #-}

module Crawl.Stats.Dice (
  Probability,
  roll,
  rollScaled,
  divRandRound,
  C,
  Dist,
  cdt,
  reverseCdt,
  probTable
) where

import Data.Ratio (Rational, (%))
import Numeric.Probability.Distribution (T, norm, decons)
import Numeric.Probability.Object (C, uniform, fromFrequencies)
import Text.PrettyPrint.Boxes ((//))
import qualified Text.PrettyPrint.Boxes as Boxes
import Text.Printf (printf)
import qualified Data.List as List

type Probability = Rational

type Dist a = T Probability a

roll :: (Monad m, C Probability m) => Integer -> m Integer
roll m | m <= 1 = return 0
       | otherwise = uniform [0..(m - 1)]

rollScaled :: (Monad m, C Probability m) => Integer -> Integer -> m Integer
rollScaled m s | m <= 0 = return 0
               | otherwise = let
                 uniOptions = [0 .. (m `div` s) - 1]
                 uniFreqs = map (\a -> (a, 1 % 1)) uniOptions
                 freqs = (m `div` s, m `mod` s % s) : uniFreqs
                 in fromFrequencies freqs

divRandRound :: (Monad m, C Probability m) => Integer -> Integer -> m Integer
divRandRound num den = fromFrequencies [(num `div` den, zeroProb), (num `div` den + 1, (1 % 1) - zeroProb)]
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
