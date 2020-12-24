{-# LANGUAGE BangPatterns #-}
module Day23 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.List
import Control.Monad.ST
import qualified Data.Array.ST as SA
import Data.Array.Unboxed
import Data.Array.Unsafe
--import qualified Data.Bimap as B

newtype Circle = Circle [Int]
  deriving Show


--data CircleB = CircleB {pos::Int, size::Int, bm:: B.Bimap Int Int}
  --deriving Show


toCircle :: Integer -> Circle
toCircle = Circle . map (read . ( :[] )) . show

--toCircleB :: Int -> Integer -> CircleB
--toCircleB mv inp = CircleB 0 mv2 (B.fromList cm)
  --where il = map (read . ( :[] )) . show $ inp
        --il2 = if maximum il < mv 
                 --then il ++ [1 + maximum il .. mv]
                 --else il
        --mv2 = max mv (maximum il)
        --cm = zip [0..] il2

example = toCircle 389125467

-- crabMoveB (CircleB p mc bm) = CircleB p2 mc bm2
--   where
--     p2 = if p == mc then 0
--                     else p+1
--     a = bm B.! p
--     dest = destIt (a-1) 
--     dpos = bm B.!> dest
-- 
--     ccups =  map (bm B.! ) . take 3 . tail . iterate ((`mod` mc) .(+ 1)) $ p
--     insertAt acc cups dest [] = error "should not happen"
--     insertAt acc cups dest (b:bs) 
--       | dest == b = acc ++ b : cups ++ bs
--       | otherwise = insertAt (acc ++ [b]) cups dest bs
--     destIt d  
--       | d `elem` ccups = destIt (d-1)
--       | d `B.memberR` bm = d
--       | otherwise = destIt mc

crabMove mv (Circle (a:b1:b2:b3:bs)) =  Circle newOrd
  where
    dest = destIt (a-1) 
    ccups = [b1,b2,b3]
    --pl = (+1) . fromJust $ elemIndex dest bs
    --c2 = take pl bs ++ ccups ++ drop pl bs ++ [a]
    destIt d  
      | d `elem` ccups = destIt (d-1)
      | d > 0 = d
      | otherwise = destIt mv
    newOrd = insertAt [] ccups dest bs ++ [a]
    insertAt acc cups dest [] = error "should not happen"
    insertAt acc cups dest (b:bs) 
      | dest == b = reverse acc ++ b : cups ++ bs
      | otherwise = insertAt (b:acc ) cups dest bs
    
toRes (Circle (c:cs))
  | c == 1 = concatMap show cs
  | otherwise = toRes (Circle (cs ++ [c]))


sol1 n ex = toRes . (!!n) . iterate (crabMove (mv ex)) $ ex
  where mv (Circle e)= maximum e

n1M = 1000000 :: Int
n1T = 1000 :: Int
toCircle2 nm inp = Circle num1M
  where nums = map (read . ( :[] )) . show $ inp
        num1M = nums ++ [1 + maximum nums .. nm]
toRes2 (Circle (c:cs))
  | c == 1 = take 2 cs
  | otherwise = toRes2 (Circle (cs ++ [c]))
sol2 nm n = toRes2 . (!!n) . iterate (crabMove nm) . toCircle2 nm

rex = sol2 n1T 10 example


-- part1 :: IO ()
part1 = print (sol1 100 . toCircle $ 942387615)
--   inp <- readData
--   print (sol2 inp)
  

-- part2 :: IO ()
-- part2 = do
--   inp <- readData
--   print (sol2 inp)

-- rewriting takes too long - try to use mutable array
-- use array:
toCircleA :: Int -> Integer -> (Int, UArray Int Int)
toCircleA nm inp = (head nums, array (1,nm) $ zip num1M (tail . cycle $ num1M))
  where nums = map (read . ( :[] )) . show $ inp
        num1M = nums ++ [1 + maximum nums .. nm]

evalArr :: Int -> Int -> UArray Int Int -> UArray Int Int
evalArr n p0 inp = SA.runSTUArray $ do
  arr <- unsafeThaw inp
  go n arr p0
  -- arr: [x,y,z,1,a,b,c,d,...,t,u,v,...]
  where 
    --go :: (SA.MArray a e m, Ix e) => a e e -> e -> m (a e e)
        go 0 m p0 =  pure m
        go n m p0 =  do pos1 <- SA.readArray m p0
                        posA <- SA.readArray m pos1
                        posB <- SA.readArray m posA
                        posC <- SA.readArray m posB
                        bounds <- SA.getBounds m
                        let selectedCups = [pos1, posA, posB] 
                            targ = findT selectedCups (p0 - 1)
                            findT dl cand 
                              | cand `elem` selectedCups = findT dl (cand -1)
                              | cand < (fst bounds) = findT dl (snd bounds)
                              | otherwise = cand
                        posAfter <- SA.readArray m targ
                        SA.writeArray m p0 posC -- 1 points to d
                        SA.writeArray m targ pos1
                        SA.writeArray m posB posAfter
                        go (n-1) m posC

(exampleAS, exampleA) = toCircleA 9 389125467
prettyArr sv arr = take 9 $ prettArr' sv arr
prettArr' sv arr = sv : prettArr' (arr ! sv) arr

sol2A inp = toRes . tail . prettyArr 1 $ evalArr (10^7) istart imap
  where (istart, imap) = toCircleA n1M  inp
        toRes (a:b:cs) = a*b

part2 = print (sol2A 942387615)

--solArr mv inp = evalArr $ toCircleA mv inp
  
