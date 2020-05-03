module Kitten where

import Snowflake

kitten :: [List] -> Polarity -> [List]
kitten [] _ = []
kitten s@(h : t) pol
  | null t && pol == polOf h =
    [revPol h]
  | null (listOf h) && (pol /= polOf h) =
    revPol h : t
  | (length s > 1) && (pol == polOf h) =
    List (polOf h) (s1 : listOf h) : t1
  | not (null $ listOf h) && (pol /= polOf h) =
    listToZList (List (polOf h) t0) : s0_0 : t
  | otherwise =
    s
  where
    (s1 : t1) = t
    (s0_0 : t0) = listOf h
