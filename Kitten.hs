module Kitten where

import Snowflake

kitten :: [List] -> Polarity -> [List]
kitten []    _   = []
kitten s@(h:t) pol =
  if null t && pol == polOf h then
    [revPol h]
  else if (null $ listOf h) && (pol /= polOf h) then
    revPol h : t
  else if (length s > 1) && (pol == polOf h) then
    List (polOf h) (s1:(listOf h)) : t1
  else if (not $ null $ listOf h) && (pol /= polOf h) then
    (listToZList $ List (polOf h) t0) : s0_0 : t
  else
    s
  where
    (s1:t1)   = t
    (s0_0:t0) = listOf h
