module Primitives where

import Fork
import Fuse
import Hokey
import Kitten
import Literal
import SaveState
import Snowflake
import Summon

safeHead :: a -> [a] -> a
safeHead d [] = d
safeHead _ as = head as

translate :: List -> [String]
translate c =
  safeHead ["nop"] $ map (\(_, _, ps) -> ps) $
    filter (\(p, l, _) -> p == pol && l == len) translations
  where
    pol = polOf c
    len = lenOf c

--      Command Root thread
exec :: List -> Thread -> Thread
exec l = execPrims (translate l) (safeHead (ZList Pos 0) $ listOf l)

--           Primitives  c0      Root thread
execPrims :: [String] -> List -> Thread -> Thread
execPrims [] _ t = t
execPrims (h : r) c0 t
  -- not the cleanest code in the world, but meh it works
  | h == "literal" = op (literal c0) Pos
  | h == "illiteral" = op (literal c0) Neg
  | h == "fuse" = op fuse Pos
  | h == "defuse" = op fuse Neg
  | h == "summon" = op summon Pos
  | h == "banish" = op summon Neg
  | h == "fork" = fork t
  | h == "spoon" = spoon t
  | h == "hokey" = op hokey Pos
  | h == "cokey" = op hokey Neg
  | h == "kitten" = op kitten Pos
  | h == "antikitten" = op kitten Neg
  | otherwise = execPrims r c0 t -- nop
  where
    op f p = execPrims r c0 $ shinyMap f p t

shinyMap :: ([List] -> Polarity -> [List]) -> Polarity -> Thread -> Thread
shinyMap f pol (Thread Shiny s ch) = Thread Shiny (f s pol) $ map (shinyMap f pol) ch
shinyMap f pol (Thread t s ch) = Thread t s $ map (shinyMap f pol) ch
