module Day13 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (minimumBy)

readData = do
  handle   <- openFile "data/Day13.txt" ReadMode
  contents <- hGetContents handle
  return $ readInput contents

readInput = fromRight undefined . parse parseTimeTable "input"

type Time = Integer
type BusLine = Integer
data TimeTable = TimeTable {baseTime :: Time, lines::[Maybe BusLine]}
  deriving Show


parseTimeTable :: GenParser Char s TimeTable
parseTimeTable = do
  t <- parseTime
  newline
  ls <- parseLines
  return (TimeTable t ls)

parseTime = read <$> many1 digit
parseLines = sepBy1 parseLine (char ',')
parseLine = (char 'x' >> return Nothing) <|> (Just <$> parseTime)


example = "939\n7,13,x,x,59,x,31,19"
ed = readInput example
input = "1000390\n23,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,383,x,x,x,x,x,x,x,x,x,x,x,x,13,17,x,x,x,x,19,x,x,x,x,x,x,x,x,x,29,x,503,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37"


nextBus baseTime bus = (1 + baseTime `div` bus) * bus

sol1 (TimeTable t bs)= res shortest
  where b2 = mapMaybe ( fmap (\b1 ->(b1, nextBus t b1 - t))) bs
        shortest = minimumBy (\(_,a) (_,b) -> compare a b) b2
        res (a,b) = a*b

-- extended euclid recursive
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b

-- example wikipedia
wex = [(0,3), (3,4), (4,5)]

-- Chinese remainder - pair
-- gcd d1 d2 = 1 -- hier in der angabe gesorgt
cr1 (r1,d1) (r2,d2) = ((r1*b2*d2 + r2*b1*d1) `mod` p, p)
  where (b1,b2) = egcd d1 d2
        p = d1 * d2
crg [x] = x
crg (x:y:xs) = crg (cr1 x y : xs)

-- Notiz aus beispielen Zeitdiff muss negativ! sein
-- e.g. (d,7)
-- t + d = 7 * i
-- ==> t + d == 0 `mod` 7
--  => t = -d `mod` 7
ex0 = [(0,17), (-2,13), (-3,19)]
ex1 = [(0,67), (-1,7), (-2,50), (-3,61)]

sol2 (TimeTable t bs)= fst crs
  where b2 = catMaybes $ zipWith (\a b -> (,) (-a) <$> b) [0..] bs
        check t (a,b) = t `mod` b == a
        solved t bl = all (check t) bl
        crs = crg b2




part1 :: IO ()
part1 = do
  print (sol1 $ readInput input)
  

part2 :: IO ()
part2 = do
  print (sol2 $ readInput input)

