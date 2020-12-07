module Day02 where
import System.IO
import           Text.ParserCombinators.Parsec.Token
import           Text.ParserCombinators.Parsec
import Data.Either (fromRight)

type Row = [Bool]
type Seat = [Bool]
data Ticket = Ticket Row Seat deriving Show
data TParse = TParse Int Int Int deriving Show

partition :: [Bool] -> Int
partition = parts 0 
  where 
    parts :: Int -> [Bool] -> Int
    parts d [] = d
    parts d ( b:bs ) = parts (2*d + if b then 1 else 0) bs


parseRow :: GenParser Char st Bool
parseRow = (char 'F' >> return False) <|> (char 'B' >> return True)
parseSeat = (char 'L' >> return False) <|> (char 'R' >> return True)
parseRowval = count 7 parseRow
parseSeatval = count 3 parseSeat
parseTicket = do
  row <- parseRowval
  seat <- parseSeatval
  return (Ticket row seat)
parseInput :: String -> [Ticket]
parseInput x = fromRight [] $ parse (sepEndBy1 parseTicket newline) "parseInput" x

readData = do
  handle   <- openFile "data/day05.txt" ReadMode
  contents <- hGetContents handle
  return $ parseInput contents

readTicket (Ticket rs ss) = TParse rn sn id
  where rn = partition rs
        sn = partition ss
        id = 8 * rn + sn

example = "BFFFBBFRRR\n\
          \FFFBBBFRRR\n\
          \BBFFBBFRLL"

getId (TParse _ _ i) = i

sol1 = maximum . map getId . map readTicket

sol2 x = free
  where ids = map getId . map readTicket $ x
        before = map (+ 1) ids
        after = map (\x -> x - 1) ids
        cands = filter (`elem` after) before
        free = filter (\x -> not (elem x ids)) cands


part1 :: IO ()
part1 = do
  inp <- readData
  print (sol1 inp)
  

part2 :: IO ()
part2 = do
  inp <- readData
  print (sol2 inp)

