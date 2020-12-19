module Day18 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)

readData =
  do handle <- openFile "data/Day18.txt" ReadMode
     contents <- hGetContents handle
     return $ readInput contents

data Operator
  = Plus
  | Mult
  deriving Show

data Value a
  = Number a
  | Bracket [Expression a]
  deriving Show

data Expression a
  = Init (Value a)
  | Operation Operator (Value a)
  deriving Show

parseVal :: GenParser Char s (Value Int)
parseVal = parseBracket <|> parseNum

parseEq :: GenParser Char s [Expression Int]
parseEq =
  do v1 <- parseVal
     vs <- many (try (spaces >> parseFOp))
     return (Init v1:vs)

parseNum = Number . read <$> many1 digit

parseBracket =
  do char '('
     spaces
     eq <- parseEq
     spaces
     char ')'
     return $ Bracket eq

parseOp = (char '+' >> return Plus) <|> (char '*' >> return Mult)

parseFOp =
  do op <- parseOp
     spaces
     v <- parseVal
     return $ Operation op v

--readInput :: String -> [Expression Int]
readInput = fromRight [] . parse (sepEndBy1 parseEq newline) "input"

example =
  "1 + 2 * 3 + 4 * 5 + 6\n\
  \1 + (2 * 3) + (4 * (5 + 6))\n\
  \2 * 3 + (4 * 5)\n\
  \5 + (8 * 3 + 9 + 3 * 4 * 3)\n\
  \5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))\n\
  \((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

ex = readInput example


eval (Init x:xs) = ev2 (toNumber x) xs
  where ev2 a [] = a
        ev2 a (Operation Plus o:os) = ev2 (a + toNumber o) os
        ev2 a (Operation Mult o:os) = ev2 (a * toNumber o) os
        toNumber (Number a) = a
        toNumber (Bracket b) = eval b

sol1 a = sum av
  where av = map eval a


eval2 (Init x:xs) = ev2 (toNumber2 x) xs
  where ev2 a [] = a
        ev2 a (Operation Plus o:os) = ev2 (a + toNumber2 o) os
        ev2 a (Operation Mult o:os) = a * ev2 (toNumber2 o) os
        toNumber2 (Number a) = a
        toNumber2 (Bracket b) = eval2 b

sol2 a = sum av
  where av = map eval2 a

part1 :: IO ()
part1 =
  do inp <- readData
     print (sol1 inp)

part2 :: IO ()
part2 =
  do inp <- readData
     print (sol2 inp)

