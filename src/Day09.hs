module Day09 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Control.Monad.State.Strict
import Data.Either (fromRight)
import Data.List (elemIndex)

example =
  "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576"

ed = readInput example

readData =
  do handle <- openFile "data/day09.txt" ReadMode
     contents <- hGetContents handle
     return $ readInput contents

readInput :: String -> [Int]
readInput = map read . lines

data Preamble =
  Preamble {nums :: [Int]
           ,sums :: [[Int]]}
  deriving Show

initPreamble nl = Preamble pns psums
  where pns = nl

        psums = bsum pns

        bsum [] = []
        bsum (n:ns) = map (n +) ns:bsum ns

addNum nu (Preamble n nl) = Preamble (n0 ++ [nu]) (nl1 ++ [[]])
  where n0 = tail n

        nl0 = tail nl

        new_sum = map (+ nu) n0

        nl1 = zipWith (:) new_sum nl0

isPresent n pre = n `elem` lnl
  where lnl = concat $ sums pre

addNs :: Int -> State Preamble ()
addNs n = modify (addNum n)

presNs :: Int -> State Preamble Bool
presNs n = gets (isPresent n)

firstBad :: [Int] -> State Preamble (Maybe Int)
firstBad [] = return Nothing
firstBad (n:ns) =
  do t <- presNs n
     if t
        then do addNs n
                firstBad ns
        else return (Just n)

confirm n nums = evalState (firstBad ncont) pr
  where nhead = take n nums

        ncont = drop n nums

        pr = initPreamble nhead

sol1 = confirm 25

cumsum = scanl1 (+)

findCont targ nl@(n:ns) =
  case res of
    Nothing -> findCont targ ns
    Just x -> Just x
  where res = srch 0 [] nl

        srch _ _ [] = Nothing
        srch s l (a:as)
          | s2 == targ = Just l2
          | s2 > targ = Nothing
          | otherwise = srch s2 l2 as
          where s2 = s + a

                l2 = a:l

weakness :: Int -> [Int] -> Maybe Int
weakness targ nl = wf <$> fc
  where fc = findCont targ nl

        wf = (+) <$> minimum <*> maximum

sol2 = undefined

part1 :: IO ()
part1 =
  do inp <- readData
     print (sol1 inp)

part2 :: IO ()
part2 =
  do inp <- readData
     let a = sol1 inp
         res =  (`weakness` inp) =<< a
     print res

