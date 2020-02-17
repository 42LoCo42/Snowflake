module Literal where

import Snowflake

literal :: List -> [List] -> Polarity -> [List]
literal c0 []    Pos   = [listToZList c0]
literal c0 []    Neg   = [revPol $ listToZList c0]
literal c0 stack pol   =
  if listOf c0 == listOf s0 then
    if pol == Pos && polOf c0 /= polOf s0 || pol == Neg then
      r0
    else
      zc0:s0:r0
  else
    if pol == Pos then
      zc0:s0:r0
    else
      revPol zc0:s0:r0
  where
    (s0:r0) = stack
    zc0 = listToZList c0
