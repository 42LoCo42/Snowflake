module Snowflake where

import System.Random

data Polarity = Pos
              | Neg
  deriving (Eq)

data List = List  Polarity [List]
          | ZList Polarity Int -- ZeroList: -3 =  -[+[]+[]+[]] = -[+0+0+0]
  deriving (Eq)

data ThreadType = Shiny
                | Tarnished
                | Shabby
                | Decrepit
  deriving (Show, Eq)

--                   Type       Stack  Children
data Thread = Thread ThreadType [List] [Thread]
  deriving Show

listToZList :: List -> List
listToZList this@(List p l)
  | canConvert       = ZList p $ length l
  | otherwise        = this
  where
    zeroList  = List Pos []
    zeroZList = ZList Pos 0
    canConvert = foldr (\a -> \r -> r && (a == zeroList || a == zeroZList)) True l

listToZList l = l

zListToList' :: List -> List
zListToList' (ZList p l) = List p $ replicate l $ List Pos []
zListToList' l = l

lenOf :: List -> Int
lenOf (List _ l)  = length l
lenOf (ZList _ l) = l

polOf :: List -> Polarity
polOf (List p _)  = p
polOf (ZList p _) = p

listOf :: List -> [List]
listOf (List _ els)   = els
listOf zl@(ZList _ _) = listOf $ zListToList' zl

revPol :: List -> List
revPol (List Pos l)  = List Neg l
revPol (List Neg l)  = List Pos l
revPol (ZList Pos l) = ZList Neg l
revPol (ZList Neg l) = ZList Pos l

instance Show Polarity where
  show Pos = "+"
  show Neg = "-"

instance Show List where
  show (ZList p l) = show p ++ show l
  show (List p es) = show p ++ "[" ++ (foldr (++) "" $ map show es) ++ "]"

nextWrap :: IO ([Int], StdGen) -> IO ([Int], StdGen)
nextWrap this = appendGen <$> this

appendGen :: ([Int], StdGen) -> ([Int], StdGen)
appendGen (nums, gen) = (new:nums, gen')
  where
    (new, gen') = next gen
