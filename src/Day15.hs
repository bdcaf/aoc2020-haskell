{-# LANGUAGE BangPatterns #-}
-- Notiz: 
-- ist Variante der Van Eck Sequenz
-- https://oeis.org/A181391
-- Muss rekursiv ausgewertet werden!
module Day15 where


import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import Data.Map.Strict as M
import Control.Monad.State.Strict as M

readData = do
  handle   <- openFile "data/Day15.txt" ReadMode
  contents <- hGetContents handle
  return $ readInput contents


example = [0,3,6]

updateMap :: Int -> Int -> Map Int [Int] -> Map Int [Int]
updateMap !k !v = alter alterF k
  where alterF Nothing = Just [v]
        alterF (Just (x:xs)) = Just [v,x]
        alterF (Just []) = Just [v]

runG t  = gInit t empty 0 
gInit :: Int -> Map Int [Int] -> Int -> [Int] -> Int
gInit !t !mem !p [a] 
  | t == p =  a 
  | otherwise = gRec t m2 p2 a
  where p2 = p+1
        m2 = updateMap a p2 mem
gInit !t !mem !p (a:as) 
  | t == p = a
  | otherwise = gInit t m2 p2 as
  where p2 = p+1
        m2 = updateMap a p2 mem

gRec :: Int -> Map Int [Int] -> Int -> Int -> Int
gRec !t !mem !pos !prev 
  | t == pos = prev
  | otherwise = gRec t m2 p2 val
  where p2 = pos +1
        m2 = updateMap val p2 mem
        findPrev = mem ! prev
        lastVal :: [Int] -> Int
        lastVal ( x:y:xs ) = x - y 
        lastVal _ = 0
        val :: Int
        val = lastVal findPrev

--speak s@(Spoken) = undefined

sol1 =  runG 2020
test1 = sol1 [0,3,6] == 436

sol2 = runG 30000000

myInput = [8,11,0,19,1,2]

part1 :: IO ()
part1 = do
  print (sol1 myInput)

part2 :: IO ()
part2 = do
  print (sol2 myInput)

