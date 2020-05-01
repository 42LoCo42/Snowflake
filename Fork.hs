module Fork where

import Snowflake

--      Root thread
fork :: Thread -> Thread
fork (Thread Shabby _ _)      = Thread Decrepit [] [Thread Shabby [] []]
fork (Thread Shiny [] _)      = Thread Tarnished [] [Thread Shabby [] []]
fork (Thread Shiny s@(h:_) _) =
  if lenOf h == 0 then
    Thread Tarnished s [Thread Shabby [] []]
  else
    Thread Tarnished s $ map (\l -> Thread Shiny (map listToZList $ listOf l) []) $ listOf h
fork (Thread t s ch) = Thread t s $ map fork ch

spoon :: Thread -> Thread
spoon (Thread Decrepit _ [Thread Shabby _ _])  = Thread Shabby [] []
spoon (Thread Tarnished s [Thread Shabby _ _]) = Thread Shiny s []
spoon (Thread Tarnished (h:t) ch) = Thread t' (listToZList (List (polOf h) s') : t) ch'
  where
    res = zipWith spoonSwitch (listOf h) ch
    t'  = if all (\(b, _, _) -> b) res then Shiny else Tarnished
    s'  = map (\(_, l, _) -> l) res
    ch' = foldr (maybeCon . (\(_, _, th) -> th)) [] res
spoon (Thread Shiny s _) = Thread Shiny [listToZList $ List Pos [listToZList $ List Pos s]] []
spoon t                  = error ("Illegal state" ++ show t)

spoonSwitch :: List -> Thread -> (Bool, List, Maybe Thread)
spoonSwitch l (Thread Shiny s _)       = (True, listToZList $ List (polOf l) s, Nothing)
spoonSwitch l t@(Thread Tarnished _ _) = (False, listToZList l, Just $ spoon t)
spoonSwitch l t                        = error ("Illegal state" ++ show l ++ " " ++ show t)

maybeCon :: Maybe a -> [a] -> [a]
maybeCon (Just a) as = a:as
maybeCon Nothing as  = as
