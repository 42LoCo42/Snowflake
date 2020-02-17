import Primitives
import Snowflake
import System.Environment
import System.Random

cycle :: (Thread, List) -> IO (Thread, List)
cycle = undefined

main = do
  args <- getArgs
  if length args == 0 then error "Specify a Snowflake program to execute!" else putStr ""
  putStrLn $ unlines args
