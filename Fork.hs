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
spoon (Thread Tarnished (h:t) ch) = Thread t' ((listToZList $ List (polOf h) s') : t) ch'
  where
    res = map (\(l, t) -> spoonSwitch l t) $ zip (listOf h) ch
    t'  = if foldr1 (&&) $ map (\(b, _, _) -> b) res then Shiny else Tarnished
    s'  = map (\(_, l, _) -> l) res
    ch' = foldr maybeCon [] $ map (\(_, _, t) -> t) res
spoon (Thread Shiny s _) = Thread Shiny [listToZList $ List Pos [listToZList $ List Pos s]] []


spoonSwitch :: List -> Thread -> (Bool, List, Maybe Thread)
spoonSwitch l (Thread Shiny s _)     = (True, listToZList $ List (polOf l) s, Nothing)
spoonSwitch l t@(Thread Tarnished _ _) = (False, listToZList l, Just $ spoon t)

maybeCon :: Maybe a -> [a] -> [a]
maybeCon (Just a) as = a:as
maybeCon Nothing as  = as
