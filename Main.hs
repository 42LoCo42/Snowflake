import Primitives
import Snowflake
import SaveState
import SaveStateCreate
import SnowflakeParser
import Parser

import System.Environment
import System.Random
import Control.Monad

flattenShiny :: Thread -> [Thread]
flattenShiny t@(Thread typ _ ch) =
  (if typ == Shiny then (t:) else id)
  (foldr (++) [] $ map flattenShiny ch)

snowflakeIO :: List -> IO List
snowflakeIO l = undefined

snowflakeCycle :: String -> Thread -> [List] -> IO ()
snowflakeCycle file root program = do
  let root' = foldl (\t -> \l -> exec l t) root program
      threads = flattenShiny root'
  -- only continue if there is at least one shiny thread left to select
  when (length threads /= 0) $ do
    ran <- (`mod` length threads) <$> abs <$> randomIO
    let thread = threads !! ran
        stack  = (\(Thread _ s _) -> s) thread

    print root
    print thread

main = do
  args <- getArgs
  when (length args == 0) $ error "Specify a Snowflake program to execute!"
  let file = head args
  contents <- readFile file
  let program = maybe [] (\(_, l) -> listOf l) $ runParser listP contents
  snowflakeCycle file (Thread Shiny [List Pos program] []) program

{-
showL :: List -> String
showL (ZList p len) = (if p == Pos then "+" else "-") ++ show len
showL (List p ls)   = (if p == Pos then "+" else "-") ++ "[" ++ (foldr1 (++) $ map showL ls) ++ "]"
-}
