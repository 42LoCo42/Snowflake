module Modify where

import Snowflake
import Primitives
import SaveState

import Data.List (sort, group, inits)
import System.Random (randomIO)

-- Abbreviate a program by deleting nops and inverses
-- This does NOT create the full "abbreviated program"!
abbrev :: [List] -> [List]
abbrev []      = []
abbrev [l]     = [l]
abbrev program
  -- invdel only does a single pass, so if something changed we abbreviate again
  | result /= program = abbrev result
  | otherwise         = result
  where
    nopdel = map fst $ filter (\(_, ts) -> ts /= ["nop"]) $
             zip program (map translate program)
    result = invdel $ nopdel
    invdel []      = []
    invdel [l]     = [l]
    invdel (l0:l1:t)
      | (polOf l0 /= polOf l1 &&
        lenOf l0 == lenOf l1) = invdel t
      | otherwise             = l0 : invdel (l1:t)

-- Zip each pair with square of count
pairsquares :: [List] -> [((List, List), Int)]
pairsquares []      = []
pairsquares [_]     = []
pairsquares program = filter (\(e, _) -> canChoose e) $
                      map (\l@(e:_) -> (e, length l ^ 2)) $ group $ sort $
                      zip program (tail program)
  where
    -- Pairs with same length cannot be chosen
    canChoose (a, b) = lenOf a /= lenOf b

-- Prepare counts for random selection
conv2prob :: [((List, List), Int)] -> [((List, List), Int)]
conv2prob program = map (\((e, _), l) -> (e, l)) $ zip program probs
  where
    probs = map (foldr1 (+)) $ tail $ inits $ map snd program

-- Select a pair
ranSel :: [((List, List), Int)] -> IO (List, List)
ranSel pairs = do
  let modul = snd $ last pairs
  num <- (`mod` modul) <$> abs <$> randomIO
  return $ fst $ head $ dropWhile (\(_, l) -> l <= num) pairs

-- Select a list from a pair
selFromPair :: (List, List) -> IO List
selFromPair (a, b) =
  if lenOf a == 1 then return b
  else if lenOf b == 1 then return a
  else do
    num <- (`mod` 2) <$> abs <$> (randomIO :: IO Int)
    if num == 0 then return a
    else return b

-- Only called if a deprecated command (and thus a pair) exists
chooseLength ::
  (List, List) ->
  List -> Maybe List ->
  [List] -> [List] ->
  IO Int

chooseLength _      _   Nothing        _       _ =
  return $ highest + 1

chooseLength (a, b) dep (Just old_dep) program full_program =
  if old_dep == a || old_dep == b then
    return $ highest + 1
  else do
    let occ_old_dep = length $ filter (== old_dep) full_program
        occ_dep     = length $ filter (== dep) program
        p0 = occ_old_dep^3 + occ_dep^3
        p1 = (occ_old_dep + occ_dep + 1) ^3
    num <- (`mod` p1) <$> abs <$> randomIO
    return $
      if num < p0 then
        highest + 1
      else
        lenOf old_dep
