module SaveStateCreate where

import Snowflake
import System.IO

save ::
  Maybe List ->
  Maybe List ->
  Maybe List ->
  Int ->
  [List] ->
  [(Polarity, Int, [String])] ->
  IO ()

save
  dep_command
  old_dep_command
  new_command
  highest
  last_programs
  translations
  = do
    file <- openFile "SaveState.hs" WriteMode
    hPutStrLn file "module SaveState where"
    hPutStrLn file "import Snowflake"
    hPutStrLn file $ "dep_command     = " ++ show dep_command
    hPutStrLn file $ "old_dep_command = " ++ show old_dep_command
    hPutStrLn file $ "new_command     = " ++ show new_command
    hPutStrLn file $ "highest         :: Int"
    hPutStrLn file $ "highest         = " ++ show highest
    hPutStrLn file $ "last_programs   = " ++ show last_programs
    hPutStrLn file "translations :: [(Polarity, Int, [String])]"
    hPutStrLn file $ "translations = " ++ show translations
    hClose file
