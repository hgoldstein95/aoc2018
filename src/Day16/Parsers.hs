module Day16.Parsers where

import Data.Array (listArray)
import Data.Either (either)
import Text.Parsec
import Text.Parsec.Char

import ElfCode.Types

type Parser a = Parsec String () a

parseSamples :: String -> [Sample]
parseSamples = either (error . show) id .
  parse (sampleP `sepBy` endOfLine) ""

parseProgram :: String -> [MachineInstr]
parseProgram = either (error . show) id .
  parse (many (machineInstrP <* endOfLine)) ""

sampleP :: Parser Sample
sampleP = Sample
  <$> (string "Before: " *> regP <* endOfLine)
  <*> (machineInstrP <* endOfLine)
  <*> (string "After:  " *> regP <* endOfLine)

regP :: Parser Regs
regP = listArray (0, 3) . fmap read <$>
  between (char '[') (char ']') (many1 digit `sepBy` string ", ")

machineInstrP :: Parser MachineInstr
machineInstrP = do
  [op, a, b, c] <- fmap read <$> many1 digit `sepBy` char ' '
  pure $ MachineInstr op a b c
