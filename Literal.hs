module Literal where

import Snowflake

literal :: List -> [List] -> Polarity -> [List]
literal c0 []    Pos   = [listToZList c0]
literal c0 []    Neg   = [revPol $ listToZList c0]
literal c0 stack pol   =
  -- Second polarity test is required for reversibility,
  -- or -1-1+1+1 would be irreversible.
  if (pol == Pos && cp /= sp && listOf c0 == listOf s0) ||
     (pol == Neg && cp == sp && listOf c0 == listOf s0) then
    r0
  else
    if pol == Pos then
      zc0:s0:r0
    else
      revPol zc0:s0:r0
  where
    (s0:r0) = stack
    zc0     = listToZList c0
    cp      = polOf c0
    sp      = polOf s0
