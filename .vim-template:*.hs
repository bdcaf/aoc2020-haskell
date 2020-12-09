module %FILE% where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)

readData = do
  handle   <- openFile "data/%FILE%.txt" ReadMode
  contents <- hGetContents handle
  return $ readInput contents

readInput :: String -> [Int]
readInput = undefined

%HERE%

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

