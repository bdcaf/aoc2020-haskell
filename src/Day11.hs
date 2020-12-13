module Day11 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import Data.Map.Strict as M
import Data.Maybe as Maybe (catMaybes, mapMaybe)
import Prelude as P

readData = do
  handle   <- openFile "data/Day11.txt" ReadMode
  contents <- hGetContents handle
  return $ toMap contents

example = "L.LL.LL.LL\n\
          \LLLLLLL.LL\n\
          \L.L.L..L..\n\
          \LLLL.LL.LL\n\
          \L.LL.LL.LL\n\
          \L.LLLLL.LL\n\
          \..L.L.....\n\
          \LLLLLLLLLL\n\
          \L.LLLLLL.L\n\
          \L.LLLLL.LL"

data PosTypes = EmptySeat | OccupiedSeat | Floor
  deriving (Eq,Ord,Show)

toType 'L' = EmptySeat
toType '#' = OccupiedSeat
toType '.' = Floor

toMap  = translate . fromList . concat . ll2 . lines
  where 
    translate  = M.map toType
    labelLine a l = zipWith (\b c -> ((a,b),c)) [0..] l
    ll2  = zipWith labelLine [0..] 
em = toMap example

surround (a,b) = [(a+as,b+bs) | as <- [-1..1],
                                bs <- [-1..1],
                                as /= 0 || bs /= 0
                ]
cases m  = Maybe.mapMaybe ( `M.lookup` m) 

change m p 
  | sm == EmptySeat = if OccupiedSeat `elem` sr 
                         then Nothing
                         else Just (p,OccupiedSeat)
  | sm == OccupiedSeat = if nocc >= 4
                            then Just (p,EmptySeat)
                            else Nothing
  | otherwise = Nothing
  where sm = m ! p
        sr = cases m (surround p)
        nocc = length . P.filter (== OccupiedSeat) $ sr

changeStep m = Maybe.mapMaybe ( m `change` ) . keys $m

recChanges m 
  | P.null cs = m
  | otherwise = recChanges (il cs m)
  where cs = changeStep m
        il [] m2 = m2
        il ((k,v):as) m2 = il as (insert k v m2)

nocc = length . M.filter (==OccupiedSeat) 

sol1 = nocc . recChanges 

part1 :: IO ()
part1 = do
  inp <- readData
  print (sol1 inp)




lineofSight p m = Maybe.mapMaybe (\d -> los p d m) cardDirs
  where cardDirs = [(a,b) | a <- [-1..1]
                          , b <- [-1..1]
                   , a /= 0 || b/=0]
        los (a0,b0) pd@(ad,bd) m =
          case p of
            Nothing -> Nothing
            Just Floor -> los p1 pd m 
            Just x -> Just  x
          where p = M.lookup p1 m
                p1 = (a0+ad, b0 + bd)

recChanges2 cf m 
  | P.null cs = m
  | otherwise = recChanges2 cf (il cs m)
  where cs = changeStep2 cf m
        il [] m2 = m2
        il ((k,v):as) m2 = il as (insert k v m2)
  
change2 m p 
  | sm == EmptySeat = if OccupiedSeat `elem` sr 
                         then Nothing
                         else Just (p,OccupiedSeat)
  | sm == OccupiedSeat = if nocc >= 5
                            then Just (p,EmptySeat)
                            else Nothing
  | otherwise = Nothing
  where sm = m ! p
        sr = lineofSight p m
        nocc = length . P.filter (== OccupiedSeat) $ sr

changeStep2 cs m = Maybe.mapMaybe ( m `cs` ) . keys $m

sol2 = nocc . recChanges2 change2

part2 :: IO ()
part2 = do
  inp <- readData
  print (sol2 inp)

