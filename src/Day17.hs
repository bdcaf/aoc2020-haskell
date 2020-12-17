{-# LANGUAGE BangPatterns #-}
module Day17 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe)

type Coord = (Int,Int,Int)
type SetCube = S.Set Coord
type Coord4 = (Int,Int,Int,Int)
type SetCube4 = S.Set Coord4

--readInput :: String -> Cube
readInput inp = S.fromList coords
  where il = lines inp
        nl = length il `div` 2
        fullIn = concat $ zipWith oneLine [-nl ..] il
        oneLine b l = zipWith (\n1 v ->((0,b,n1), v) ) [-nl ..]  $ readLine l
        readLine [] = []
        readLine ('#':xs) = True: readLine xs
        readLine ('.':xs) = False: readLine xs
        coords = map fst . filter snd $ fullIn
to4 :: SetCube -> SetCube4
to4 = S.map (\(a,b,c) -> (0,a,b,c)) 

input = "##.#####\n\
        \#.##..#.\n\
        \.##...##\n\
        \###.#...\n\
        \.#######\n\
        \##....##\n\
        \###.###.\n\
        \.#.#.#.."
example = ".#.\n\
          \..#\n\
          \###"

prettyCube c = putStr $ concatMap sliceZ (rv toZ cl) 
  where 
        sliceZ z = '[':show z ++ "]\n" ++ concatMap (sliceY z) (rv toY cl)
        sliceY z y = map (\x -> pretty $ (z,y,x) `S.member` c) (rv toX cl) ++ "\n"
        pretty True = '#'
        pretty False = '.'
        cl = S.toList c
        rv f xs = [mi..ma]
          where vl = map f xs
                mi = minimum vl
                ma = maximum vl
        toZ (x,_,_) = x
        toY (_,x,_) = x
        toX (_,_,x) = x

neighbors !cc !k = if k `S.member` cc then allN - 1 else allN
  where
    allN = length . filter (`isNeighbor` k) $ S.toList cc
    isNeighbor :: Coord -> Coord -> Bool
    isNeighbor (a1,b1,c1) (a2,b2,c2) = 
      isN1 a1 a2 && isN1 b1 b2 && isN1 c1 c2
    isN1 a1 a2 = abs (a1 - a2) <=1

neighbor4 !cc !k = if k `S.member` cc then allN - 1 else allN
  where
    allN = length . filter (`isNeighbor` k) $ S.toList cc
    isNeighbor (a1,b1,c1,d1) (a2,b2,c2,d2) = 
      isN1 a1 a2 && isN1 b1 b2 && isN1 c1 c2 && isN1 d1 d2
    isN1 a1 a2 = abs (a1 - a2) <=1

cands !c0 = S.difference c1 c0
  where c1 = expand c0
expand !c0 = S.fromList c1
  where c1 = concatMap el (S.toList c0)
        el (a,b,c) = [(a+i,b+j,c+k)|i <- l1, j<-l1, k<-l1]
        l1 = [-1..1]

cand4 !c0 = S.difference c1 c0
  where c1 = expand4 c0
expand4 !c0 = S.fromList c1
  where c1 = concatMap el (S.toList c0)
        el (a,b,c,d) = [(a+i,b+j,c+k, d+l)|
          i <- l1, j<-l1, k<-l1, l<-l1]
        l1 = [-1..1]


evolve :: SetCube -> SetCube
evolve !cc =  cc2 `S.union` cand2
  where 
    cand = cands cc
    newCand !co = neighbors cc co == 3
    keepCand !co = let n = neighbors cc co in 
                      n == 3 || n==2
    cc2 = S.filter keepCand cc
    cand2 = S.filter newCand cand

evolve4 !cc =  cc2 `S.union` cand2
  where 
    cand = cand4 cc
    newCand !co = neighbor4 cc co == 3
    keepCand !co = let n = neighbor4 cc co in 
                      n == 3 || n==2
    cc2 = S.filter keepCand cc
    cand2 = S.filter newCand cand


sol1 = S.size . (!!6) . iterate evolve 

sol2 = S.size . (!!6) . iterate evolve4 . to4


part1 :: IO ()
part1 = do
  print (sol1 $ readInput input)
  

part2 :: IO ()
part2 = do
  print (sol2 $ readInput input)

