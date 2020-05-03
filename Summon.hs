module Summon where

import Snowflake

summon :: [List] -> Polarity -> [List]
summon (s0 : s1 : s2 : r) pol =
  if pol == Pos
    then s2 : s0 : s1 : r
    else s1 : s2 : s0 : r
summon s _ = s
