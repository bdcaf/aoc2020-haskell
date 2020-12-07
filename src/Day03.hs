module Day02 where
import System.IO
import           Text.ParserCombinators.Parsec.Token
import           Text.ParserCombinators.Parsec
import Data.Either (fromRight)
import Data.Algebra.Boolean (xor)

exampleInput = lines
  "..##.......\n\
  \#...#...#..\n\
  \.#....#..#.\n\
  \..#.#...#.#\n\
  \.#...##..#.\n\
  \..#.##.....\n\
  \.#.#.#....#\n\
  \.#........#\n\
  \#.##...#...\n\
  \#...##....#\n\
  \.#..#...#.#"

type TreeMap = [String]


data Movement = Movement Int Int
  deriving Show


readData :: IO TreeMap
readData = do
  handle   <- openFile "data/day03.txt" ReadMode
  contents <- hGetContents handle
  return $ lines contents

rotate = drop <> take

moveRight :: Int -> TreeMap -> TreeMap
moveRight s = map (rotate s)

step (Movement r d)  = moveRight r . drop d 

locSymb = head . head
encounters m [] = []
encounters m s = locSymb s : encounters m (step m s)

numTrees = length . filter (=='#')

sol1 m = numTrees . encounters m

checkMultiple ms tm = map (`sol1` tm) ms
mlist = [ Movement 1 1
        , Movement 3 1
        , Movement 5 1
        , Movement 7 1
        , Movement 1 2]
      
sol2 = product . checkMultiple mlist 


part1 :: IO ()
part1 = do
  tm <- readData
  let res = sol1 (Movement 3 1) tm
  print res

part2 :: IO ()
part2 = do
  tm <- readData
  let res = sol2 tm
  print res
