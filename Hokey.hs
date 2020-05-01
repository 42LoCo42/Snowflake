module Hokey where

import Snowflake
import Data.List

-- Entrypoint, gets stack
hokey :: [List] -> Polarity -> [List]
hokey [] _ = []
hokey (h:t) pol
  | pol == Pos
  = listToZList (List (polOf h) $ ignoReve $ segmTrns $ listOf h) : t
  | otherwise
  = listToZList (List (polOf h) $ segmTrns $ ignoReve $ listOf h) : t

-- Subroutine, gets first stack element
segmTrns :: [List] -> [List]
segmTrns l
  | special   = l
  | otherwise = flatten $ map transposeSNF segments
  where
    segments = groupBy (\l0 l1 -> (lenOf l0 == lenOf l1) && (polOf l0 == polOf l1)) l
    special  = isSpecial segments

flatten :: [[x]] -> [x]
flatten xss = [x | xs <- xss, x <- xs]

transposeSNF :: [List] -> [List]
transposeSNF s
  | lenOf s0 == 0 = s -- n lists of zero elements can't be transposed
  | otherwise     = s'
  where
    (s0:_) = s
    s'     = map (listToZList . List (polOf $ head s) . map listToZList) $ transpose $ map listOf s

isSpecial :: [[List]] -> Bool
isSpecial []      = False
isSpecial [_]     = False
isSpecial (s0:r0) =
  (length s0 == length s1 && polOf s0_0 == polOf s1_0 &&
  lenOf s0_0 /= 0 && lenOf s1_0 /= 0) ||
  isSpecial r0
  where
    (s1:_)   = r0
    (s0_0:_) = s0
    (s1_0:_) = s1

-- Subroutine, gets first stack element
ignoReve :: [List] -> [List]
ignoReve l = place trg els
  where
    trg = map (\li -> ZList (polOf li) (lenOf li)) l
    els = reverse $ concatMap listOf l

--       Target    Elements  Result
place :: [List] -> [List] -> [List]
place []     _  = []
place _      [] = []
place (t:ts) es = listToZList (List (polOf t) (map listToZList $ take c es)) : place ts (drop c es)
  where
    c = lenOf t
