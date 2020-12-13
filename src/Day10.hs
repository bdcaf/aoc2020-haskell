module Day10 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import Data.List 

example = "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"
ed = readInput example

readData = do
  handle   <- openFile "data/Day10.txt" ReadMode
  contents <- hGetContents handle
  return $ readInput contents



readInput :: String -> [Int]
readInput = map read . lines

diff  = zipWith (-) <$> tail <*> id
--diff [] = []
--diff [_] = []
--diff (a:b:c) = (b-a) : diff (b:c)

chain = sort
pad xs = 0:xs++[ 3+maximum xs ]


sol1 :: [Int] -> Int
sol1 inp = d1 * d3 
  where ci = chain inp
        ds = diff ci
        d1 = length . filter (==1) $ ds
        d3 = length . filter (==3) $ ds
      

ways :: [Int] -> [Int] -> [[Int]]
ways pre (a:b:c:cs)  
  | c-a <= 3  = ways pre2 (b:c:cs) ++ ways pre (a:c:cs)
  | otherwise = ways pre2 (b:c:cs)
  where pre2 = pre ++ [a]
ways pre x = [pre ++ x]

qways  (a:b:c:cs)  
  | c-a <= 3  = qways (b:c:cs) + qways  (a:c:cs)
  | otherwise = qways (b:c:cs)
qways  x = 1

-- uses that only series of differences of 1 can be replaced.
-- 1 eg. 125 -> 1 (125)
-- 11 eg. 1236 -> 1236 oder 136 (2x)
-- 111 -> 12347 -> 12347, 1347, 1247, 147 (=4x)
-- Formel unsicher
--
-- idee vllt - nur bereiche mit mit abständen kleiner 3 ansehen
tways dl = product $ map (pos!!) gd
  where diffs = diff dl
        gd = map length . filter (( ==1 ) . head) $ group diffs
        pos = 1:1:2:zipWith3 (\a b c -> a+b+c) pos (tail pos) (tail (tail pos))

-- Eigene Lösung - teilt in kleinere Regionen,
-- separiert bei sprüngen von 3 - die nur auf einem Weg erreicht werden können
splt :: [Int] -> [Int] -> [[Int]]
splt  pr (a0:a:as)
  | a-a0 ==3  = (pr ++ [a0]) : splt  [] (a:as)
  | otherwise = splt  (pr ++ [a0]) (a:as)
splt  pr x = [pr++x]

mySol inp = product poss
  where sers = splt [] inp
        poss = map qways sers


--sol2 = length . ways [] . sort . pad
--
sol2 =  mySol . sort . pad


part1 :: IO ()
part1 = do
  inp <- readData
  print (sol1 inp)
  

part2 :: IO ()
part2 = do
  inp <- readData
  print (sol2 inp)

