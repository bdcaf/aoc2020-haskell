module Day16 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import Data.Maybe (mapMaybe)

data Note = Note {nConstr::[Constraint]
                 ,myTicket::Ticket 
                 ,nearbyTicket:: [Ticket]
                 }
  deriving Show

type Range1 = (Int,Int)
data Constraint = Constraint String [Range1]
  deriving Show
newtype Ticket = Ticket {unTicket::[Int]}
  deriving Show

parseNoteSection = sepEndBy1 parseNoteLine newline
parseNoteLine = do
  label <- manyTill (noneOf "\n") (try (char ':'))
  spaces
  ranges <- sepBy parseRange (try (spaces >> string "or" >> spaces))
  return (Constraint label ranges)
parseDig = read <$> many1 digit
parseRange = do
  lo <- parseDig
  char '-'
  hi <- parseDig
  return (lo,hi)

parseNote = do
  ns <- parseNoteSection
  newline
  string "your ticket:"
  newline
  mt <- parseTicket
  newline
  newline
  string "nearby tickets:"
  newline
  others <- sepEndBy1 parseTicket newline
  return $ Note ns mt others


parseTicket= Ticket <$> sepBy1 parseDig (try (char ','))

  
readData = do
  handle   <- openFile "data/Day16.txt" ReadMode
  contents <- hGetContents handle
  return $ readInput contents

readInput = fromRight undefined . parse parseNote "" 


example = "class: 1-3 or 5-7\n\
          \row: 6-11 or 33-44\n\
          \seat: 13-40 or 45-50\n\
          \\n\
          \your ticket:\n\
          \7,1,14\n\
          \\n\
          \nearby tickets:\n\
          \7,3,47\n\
          \40,4,50\n\
          \55,2,20\n\
          \38,6,12"
ex1 = readInput example
ex2 = readInput "class: 0-1 or 4-19\n\
                \row: 0-5 or 8-19\n\
                \seat: 0-13 or 16-19\n\
                \\n\
                \your ticket:\n\
                \11,12,13\n\
                \\n\
                \nearby tickets:\n\
                \3,9,18\n\
                \15,1,5\n\
                \5,14,9"

checkConstraint (Constraint str []) _ = False
checkConstraint (Constraint str ((a,b):cs)) v =  
  (a <= v && v <= b) || checkConstraint (Constraint str cs) v
anyConstraint cs v = any (`checkConstraint` v) cs

checkTicket cs (Ticket vs) = all (anyConstraint cs) vs
errorRate cs (Ticket vs) = v2
  where v2 = filter (not . anyConstraint cs) vs

sol1 inp = sum $ concatMap (errorRate cs) (nearbyTicket inp)
  where cs = nConstr inp



part1 :: IO ()
part1 = do
  inp <- readData
  print (sol1 inp)
  

canBe :: Constraint -> Int -> Maybe String
canBe (Constraint str []) _ = Nothing
canBe (Constraint str ((a,b):cs)) v   
  | a <= v && v <= b = Just str
  | otherwise = canBe (Constraint str cs) v

ticketCand :: Note -> [[String]]
ticketCand ns = ticketCand' cs vt
  where cs = nConstr ns
        ts = (myTicket ns) : filter (checkTicket cs) (nearbyTicket ns)
        vt = map unTicket ts

ticketCand' _ [] = []
ticketCand' cs ts 
  | null (head ts) = []
  | otherwise = candColl : ticketCand' cs fol
  where cur = map head ts
        fol = map tail ts
        cands1 :: Int -> [String]
        cands1 v = mapMaybe (`canBe` v) cs
        curCands = map cands1 cur
        coll a b = filter (`elem` b) a
        candColl = foldl1 coll curCands

cleanCands :: [[String]] -> [String]
cleanCands tl 
  | maximum (map length tl) == 1 = map head tl
  | otherwise = cleanCands cleaned
    where 
          cleanf :: String -> [String] -> [String]
          cleanf w t2 
            | length t2 == 1 = t2
            | otherwise = filter (/= w) t2
          goods :: [String] 
          goods = map head $ filter (\a -> length a == 1) tl
          --cleaned :: [[String]]
          cleaned = foldr c1 tl goods
          c1 :: String -> [[String]] -> [[String]]
          c1 w = map (cleanf w)
  
matchStart s1 s2 = s1==subS2
  where subS2 = take (length s1) s2

sol2 inp = res
  where tc = ticketCand inp
        tn = cleanCands tc
        m = zip tn $ unTicket $ myTicket inp
        mDep = filter (matchStart "departure" . fst) m
        res = product . map (snd) $ mDep

part2 :: IO ()
part2 = do
  inp <- readData
  print (sol2 inp)

