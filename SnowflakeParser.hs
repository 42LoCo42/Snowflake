module SnowflakeParser where

import Parser
import Snowflake
import Control.Applicative
import Data.Char

polarityP :: (Polarity -> a -> List) -> Parser (a -> List)
polarityP typ =
  ((\_ -> typ Pos) <$> charP '+') <|>
  ((\_ -> typ Neg) <$> charP '-')

rawListP :: Parser [List]
rawListP = charP '[' *> many listP <* charP ']'

numP :: Parser Int
numP = read <$> spanP isNumber

listP :: Parser List
listP = (polarityP List <*> rawListP) <|>
        (polarityP ZList <*> numP)
