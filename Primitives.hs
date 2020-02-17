module Primitives where

import Snowflake
import Literal
import Fuse
import Summon
import Fork
import Hokey
import Kitten

translations :: [(Polarity, Int, [String])]
translations = [(Pos, 1, ["literal"]), (Neg, 1, ["illiteral"]),
                (Pos, 2, ["fuse"]),    (Neg, 2, ["defuse"]),
                (Pos, 3, ["summon"]),  (Neg, 3, ["banish"]),
                (Pos, 4, ["fork"]),    (Neg, 4, ["spoon"]),
                (Pos, 5, ["hokey"]),   (Neg, 5, ["cokey"]),
                (Pos, 6, ["kitten"]),  (Neg, 6, ["antikitten"])]

safeHead :: a -> [a] -> a
safeHead d [] = d
safeHead _ as  = head as

translate :: List -> [String]
translate c = safeHead ["nop"] $ map (\(_,_,ps) -> ps) $
              filter (\(p, l, _) -> p == pol && l == len) translations
  where
    pol = polOf c
    len = lenOf c

--      Command Root thread
exec :: List -> Thread -> Thread
exec l t = execPrims (translate l) (safeHead (ZList Pos 0) $ listOf l) t

--           Primitives  c0      Root thread
execPrims :: [String] -> List -> Thread -> Thread
execPrims []    _  t = t
execPrims (h:r) c0 t =
-- not the cleanest code in the world, but meh it works
  if h == "literal"         then op (literal c0) Pos
  else if h == "illiteral"  then op (literal c0) Neg
  else if h == "fuse"       then op fuse Pos
  else if h == "defuse"     then op fuse Neg
  else if h == "summon"     then op summon Pos
  else if h == "banish"     then op summon Neg
  else if h == "fork"       then fork t
  else if h == "spoon"      then spoon t
  else if h == "hokey"      then op hokey Pos
  else if h == "cokey"      then op hokey Neg
  else if h == "kitten"     then op kitten Pos
  else if h == "antikitten" then op kitten Neg
  else execPrims r c0 t -- nop
  where
    op = (\f -> \p -> execPrims r c0 $ shinyMap f p t)

shinyMap :: ([List] -> Polarity -> [List]) -> Polarity -> Thread -> Thread
shinyMap f pol (Thread Shiny s ch) = Thread Shiny (f s pol) $ map (shinyMap f pol) ch
shinyMap f pol (Thread t s ch) = Thread t s $ map (shinyMap f pol) ch
