import Primitives
import Snowflake
import SnowflakeParser
import Parser
import Modify
import SaveState
import SaveStateCreate

import System.Environment (getArgs)
import System.IO (hSetBuffering, stdin, BufferMode (NoBuffering))
import System.Random (randomIO)
import Control.Monad (when)
import Data.Char (ord, chr)

flattenShiny :: Thread -> [Thread]
flattenShiny t@(Thread typ _ ch) =
  (if typ == Shiny then (t:) else id)
  (foldr (++) [] $ map flattenShiny ch)

main = do
  hSetBuffering stdin NoBuffering
  args <- getArgs
  when (length args == 0) $ error "Specify a Snowflake program to execute!"
  let file = head args
  contents <- readFile file
  let program = maybe [] (\(_, l) -> listOf l) $ runParser listP contents
  snowflakeCycle file (Thread Shiny [List Pos program] []) program

-- Execution model
snowflakeCycle :: String -> Thread -> [List] -> IO ()
snowflakeCycle file root program = do
  let root'          = foldl (\t -> \l -> exec l t) root program
      threads        = flattenShiny root'
      full_program   = abbrev program ++ last_programs

  -- only continue if there is at least one shiny thread left to select
  if length threads /= 0 then do
    ran <- (`mod` length threads) <$> abs <$> randomIO
    let thread = threads !! ran
        stack  = (\(Thread _ s _) -> s) thread

    -- Perform IO
    stack' <- if length stack < 2 then return stack else do
      let (h0:h1:t) = stack
      h1'  <- if polOf h1 == Neg then do
                c <- getChar
                return $ listToZList $ List Neg $ listOf h1 ++ [ZList Pos $ ord c]
              else if lenOf h1 > 0 then do
                print $ chr $ lenOf $ head $ listOf h1
                return $ listToZList $ List Pos $ tail $ listOf h1
              else do
                return h1
      return (h0:h1':t)

    -- Perform language changes
    let prob_pairs = conv2prob $ pairsquares full_program
    stack'' <- if length full_program < 2 ||
                  null prob_pairs then do
      save
        dep_command
        old_dep_command
        new_command
        highest
        full_program
        translations
      return stack'
    else do
      -- Update deprecated commands
      pair@(a, b) <- ranSel prob_pairs
      new_dep_command <- selFromPair pair
      let new_old_dep_command = (dep_command :: Maybe List)

      -- Generate new command
      new_length <- chooseLength
        pair
        new_dep_command new_old_dep_command
        program full_program
      let new_translation = translate a ++ translate b
          inv_translation = translate (revPol b) ++ translate (revPol a)
      pos_first <- (== 0) <$> (`mod` 2) <$> abs <$> (randomIO :: IO Int)
      let fst_pol     = if pos_first then Pos else Neg
          snd_pol     = if pos_first then Neg else Pos
          new_command = ZList fst_pol new_length
          new_highest =
            if new_length <= highest then
              highest
            else
              new_length

      -- Update translations
      -- By filtering we replace existing commands with that length
          new_translations =
            (fst_pol, new_length, new_translation) :
            (snd_pol, new_length, inv_translation) :
            filter (\(_, l, _) -> l /= new_length) translations

      -- Save everything
      save
        (Just new_dep_command)
        new_old_dep_command
        (Just new_command)
        new_highest
        []
        new_translations

      print pair
      print new_dep_command
      print new_old_dep_command
      print new_length
      print new_translation
      print inv_translation

      -- Create new stack
      return $ stack' ++ [List (polOf $ head stack') [
        maybe (ZList Pos 0) id new_old_dep_command,
        new_dep_command,
        a,
        b,
        new_command
        ]]

    print stack
    print stack''
  else do -- execution ends, but we have to append the current program
    save
      dep_command
      old_dep_command
      new_command
      highest
      full_program -- here
      translations

{-
showL :: List -> String
showL (ZList p len) = (if p == Pos then "+" else "-") ++ show len
showL (List p ls)   = (if p == Pos then "+" else "-") ++ "[" ++ (foldr1 (++) $ map showL ls) ++ "]"
-}
