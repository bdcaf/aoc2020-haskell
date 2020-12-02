module Day02 where
import System.IO
import           Text.ParserCombinators.Parsec.Token
import           Text.ParserCombinators.Parsec
import Data.Either (fromRight)
import Data.Algebra.Boolean (xor)

data Policy = Policy Char Int Int
  deriving Show

data Entry = Entry Policy String
  deriving Show

readData = do
  handle   <- openFile "data/day02.txt" ReadMode
  contents <- hGetContents handle
  return $ fromRight [] $ parse parseInput "read data"  contents

parseInt :: GenParser Char st Int
parseInt = read <$> many1 digit

parsePolicy :: GenParser Char st Policy
parsePolicy = do
  low <- parseInt
  char '-'
  high <- parseInt
  char ' '
  x <- anyChar
  return (Policy x low high)

parseEntry = do
  pol <- parsePolicy
  char ':'
  many1 (char ' ')
  pwd <- many1 lower
  return (Entry pol pwd)

parseInput = sepEndBy1 parseEntry newline
  
isValid (Policy c mi ma) pwd = mi <= lce && lce <= ma
  where ce = filter (== c) pwd
        lce = length ce

isValidB (Policy c mi ma) pwd = (la == c)  `xor` (lb == c)
  where la = pwd!!(mi-1)
        lb = pwd!!(ma-1)

valEntry (Entry p s) = isValid p s
valEntryB (Entry p s) = isValidB p s

part1 :: IO ()
part1 = do
  inp <- readData
  let res = length . filter valEntry $ inp
  print res

part2 :: IO ()
part2 = do
  inp <- readData
  let res = length . filter valEntryB $ inp
  print res
