module Fuse where

import Snowflake

data FuseType
  = FList List
  | FPol Polarity
  deriving (Show, Eq)

fuse :: [List] -> Polarity -> [List]
fuse [] _ = []
fuse (zl@(ZList _ _) : t) pol = fuse (zListToList' zl : t) pol
fuse ((List lpol ls) : t) pol = List lpol (fusePost $ fuseScan pol $ fusePre ls) : t

fuseSingle :: List -> [FuseType]
fuseSingle l@(ZList _ _) = [FList l]
fuseSingle (List pol list) = FPol pol : map FList list

fusePre :: [List] -> [FuseType]
fusePre = concatMap fuseSingle

fuseScan :: Polarity -> [FuseType] -> [FuseType]
fuseScan pol ((FList f0) : l1@(FPol f1) : l2@(FList f2) : r)
  | pol == Pos =
    if (polOf f0 == Pos) && (polOf f2 == Neg)
      then
        if f1 == Pos
          then FList (revPol f0) : FList (revPol f2) : fuseScan pol r -- +(+)- to -+
          else FList (revPol f0) : FPol Pos : FList (revPol f2) : fuseScan pol r -- +(-)- to -(+)+
      else FList (revPol f0) : l1 : fuseScan pol (l2 : r)
  | (polOf f0 == Neg) && (polOf f2 == Pos) =
    if f1 == Neg
      then FList (revPol f0) : FList (revPol f2) : fuseScan pol r -- -(-)+ to +-
      else FList (revPol f0) : FPol Neg : FList (revPol f2) : fuseScan pol r -- -(+)+ to +(-)-
  | otherwise =
    FList (revPol f0) : l1 : fuseScan pol (FList f2 : r)
fuseScan pol ((FList f0) : l1@(FList f1) : r)
  | pol == Pos && (polOf f0 == Pos) && (polOf f1 == Neg) =
    FList (revPol f0) : FPol Neg : FList (revPol f1) : fuseScan pol r -- +- to -(-)+
  | pol == Neg && (polOf f0 == Neg) && (polOf f1 == Pos) =
    FList (revPol f0) : FPol Pos : FList (revPol f1) : fuseScan pol r -- -+ to +(+)-
  | otherwise =
    FList (revPol f0) : fuseScan pol (l1 : r)
fuseScan pol ((FList f) : t) = FList (revPol f) : fuseScan pol t
fuseScan pol (h : t) = h : fuseScan pol t
fuseScan _ [] = []

fusePost :: [FuseType] -> [List]
fusePost ((FPol p) : r) =
  List p (map (\(FList l) -> l) $ takeWhile fuseTypeIsList r)
    : fusePost (dropWhile fuseTypeIsList r)
fusePost ((FList l) : r) = l : fusePost r
fusePost [] = []

fuseTypeIsList :: FuseType -> Bool
fuseTypeIsList (FList _) = True
fuseTypeIsList _ = False
