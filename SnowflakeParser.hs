module SnowflakeParser where

import Parser
import Snowflake
import Control.Applicative
import Data.Char

polarityP :: Parser ([List] -> List)
polarityP =
  ((\_ -> List Pos) <$> charP '+') <|>
  ((\_ -> List Neg) <$> charP '-')

polarityPZ :: Parser (Int -> List)
polarityPZ =
  ((\_ -> ZList Pos) <$> charP '+') <|>
  ((\_ -> ZList Neg) <$> charP '-')

rawListP :: Parser [List]
rawListP = charP '[' *> many listP <* charP ']'

listP :: Parser List
listP = (polarityP <*> rawListP) <|>
        (polarityPZ <*> (read <$> spanP isNumber))
