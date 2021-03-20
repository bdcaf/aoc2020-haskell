module Day25 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

readData = do
  handle   <- openFile "data/Day25.txt" ReadMode
  contents <- hGetContents handle
  return $ readInput contents

readInput = fromRight undefined . parse undefined "input" 

example = (17807724,5764801)
input = (13316116, 13651422)

lstep s v = (s * v) `mod` 20201227
kpubkey k s = elemIndex k . iterate (lstep s) $ 1

sol1 (dk, ck) = common
  where kl = kpubkey dk 7
        common = (!! fromJust kl) . iterate (lstep ck) $ 1
  
-- sol1 = 12929

sol2 = undefined


-- part1 :: IO ()
-- part1 = do
--   inp <- readData
--   print (sol2 inp)
  

-- part2 :: IO ()
-- part2 = do
--   inp <- readData
--   print (sol2 inp)

