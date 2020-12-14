module Day14 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import Data.Map.Strict as M
import Control.Monad.State.Strict as S

readData = do
  handle   <- openFile "data/Day14.txt" ReadMode
  contents <- hGetContents handle
  return $ readInput contents

example = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
          \mem[8] = 11\n\
          \mem[7] = 101\n\
          \mem[8] = 0"

readInput :: String -> [Command]
readInput = fromRight [] . parse parseProg "input" 

parseProg :: GenParser Char s [Command]
parseProg = sepEndBy1 parseLine newline
  where
    parseLine = (try parseMaskCommand) <|> parseMemCommand

    parseMaskCommand :: GenParser Char s Command
    parseMaskCommand = do
      string "mask"
      spaces
      char '='
      spaces
      SetMask <$> parseMask

    parseMemCommand :: GenParser Char s Command
    parseMemCommand = do
      string "mem"
      char '['
      pos <- read <$> many1 digit
      char ']'
      spaces
      char '='
      spaces
      val <- read <$> many1 digit
      return (SetAddr pos val)

    parseMask :: GenParser Char s Mask
    parseMask = Mask <$> many1 (pNot <|> pT <|> pf)
      where pNot = char 'X' >> return Nothing
            pT = char '1' >> return  (Just True)
            pf = char '0' >> return  (Just False)

data Setter = Setter Int Integer
  deriving Show
newtype Mask = Mask [Maybe Bool]
  deriving Show

data Command = SetMask Mask | SetAddr Int Integer
  deriving Show

type Bits = [Bool]
data Machine = Machine {mMask::Mask, mem::Map Int Bits}
  deriving Show

initMachine = Machine (Mask $ replicate 32 Nothing) empty

execute :: Command -> State Machine ()
execute (SetMask m) = modify (\ma -> ma {mMask = m})
execute (SetAddr add val) = undefined
  where bval = toFullBits val
  

toFullBits n = (replicate (36 - length b0) False) ++ b0
  where b0 = toBits n

toBits 0 = []
toBits n = toBits q ++ [r == 1]
  where (q,r) = divMod n 2


fromBits :: [Bool] -> Integer
fromBits bs = go 0 bs
  where
    go c [] = c
    go c (True:xs) = go (2*c + 1) xs
    go c (False:xs) = go (2*c) xs


sol1 = undefined

sol2 = undefined


part1 :: IO ()
part1 = do
  inp <- readData
  undefined
  

part2 :: IO ()
part2 = do
  inp <- readData
  undefined

