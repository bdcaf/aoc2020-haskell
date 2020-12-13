module Day12 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import Control.Monad.State.Strict as S

readData = do
  handle   <- openFile "data/Day12.txt" ReadMode
  contents <- hGetContents handle
  return $ readInput contents

readInput :: String -> [Action]
readInput m = fromRight [] $ parse (sepEndBy1 parseAction newline) "input" m
parseAction :: GenParser Char s Action
parseAction = do 
  ac <- choice [north, south, east, west, left, right, forward]
  val <- read <$> many1 digit
  return (ac val)
    where 
      north = char 'N' >> return (Move North)
      south = char 'S' >> return (Move South)
      east = char 'E' >> return (Move East)
      west = char 'W' >> return (Move West)
      left = char 'L' >> return MLeft
      right = char 'R' >> return MRight
      forward = char 'F' >> return Forward




type Distance = Int
type Degree = Int
data Direction = North  | East | South | West
  deriving (Show, Enum)
data Action = Move Direction Distance
             | MLeft Degree
             | MRight Degree
             | Forward Distance
             deriving Show

type Position =  (Distance, Distance)

data Ship = Ship {pos::Position,dir:: Direction}
  deriving Show

type Waypoint = (Distance, Distance)
data Ship2 = Ship2 {pos2::Position, wp2:: Waypoint}
  deriving Show

move :: Action -> Ship -> Ship
move a (Ship p dir)  =
  case a of
    (MLeft deg) -> Ship p (dirF (-) dir (toDir deg))
    (MRight deg) -> Ship p (dirF (+) dir (toDir deg))
    (Move mdir dist) -> Ship (trans mdir dist p) dir
    (Forward  dist) -> Ship (trans dir dist p) dir
  where 
    toDir :: Degree -> Int
    toDir d = d `div` 90
    dirF f d0 d1 = toEnum (( fromEnum d0 `f` d1) `mod` 4)
    trans North d (x,y) = (x-d, y)
    trans South d (x,y) = (x+d, y)
    trans East d (x,y) = (x, y+d)
    trans West d (x,y) = (x, y-d)

moveShip :: Action -> State Ship ()
moveShip a = modify (move a)

course :: [Action] -> State Ship ()
course = foldr ((>>) . moveShip) (return ())

 
example = "F10\nN3\nF7\nR90\nF11"
ed = readInput example

dist (x,y) = abs x+ abs y

sol1 as =  dist . pos $ execState (course as) (Ship (0,0) East)

move2 :: Action -> Ship2 -> Ship2
move2 a (Ship2 p wp)  =
  case a of
    (MLeft deg) -> Ship2 p (rotate ((-deg) `mod` 360) wp)
    (MRight deg) -> Ship2 p (rotate (deg `mod` 360) wp)
    (Move mdir dist) -> Ship2 p (trans mdir dist wp) 
    (Forward  dist) -> Ship2 (forward dist wp p) wp
  where 
    rotate 0 w = w
    rotate 90 (wx,wy) = (-wy, wx)
    rotate 180 (wx,wy) = (-wx, -wy)
    rotate 270 (wx,wy) = (wy, -wx)
    trans North d (x,y) = (x+d, y)
    trans South d (x,y) = (x-d, y)
    trans East d (x,y) = (x, y+d)
    trans West d (x,y) = (x, y-d)
    forward d (wx,wy) (x,y) = (x + d*wx, y+ d*wy)

moveShip2 :: Action -> State Ship2 ()
moveShip2 a = modify (move2 a)

course2 :: [Action] -> State Ship2 ()
course2 = foldr ((>>) . moveShip2) (return ())

sol2 as =  dist . pos2 $ execState (course2 as) (Ship2 (0,0) (1,10) )


part1 :: IO ()
part1 = do
  inp <- readData
  print (sol1 inp)
  

part2 :: IO ()
part2 = do
  inp <- readData
  print (sol2 inp)

