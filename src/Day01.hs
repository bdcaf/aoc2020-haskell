{-# LANGUAGE TupleSections #-}
module Day01 where
import System.IO

readData :: IO [Integer]
readData = do
  handle   <- openFile "data/day01.txt" ReadMode
  contents <- hGetContents handle
  return $ fmap read (lines contents)

findProd t [] = []
findProd t [_] = []
findProd t (a:as) = go a [] as ++ findProd t as 
  where go f res [] = res
        go f res (b:bs) = if (a+b) == t 
                             then (a,b): go f res bs
                             else go f res bs

toRes (a,b) = a*b

group2 [] = []
group2 [_] = []
group2 (a:as) = map (a,) as ++ group2 as
group3 [] = []
group3 [_] = []
group3 (a:as) = map (\(b,c) ->(a,b,c)) (group2 as) ++ group3 as

test3 t (a,b,c) = a+b+c == t

sol3  = map (\(a,b,c) -> a*b*c) . filter (test3 2020) . group3


sample = [1721 ,979 ,366 ,299 ,675 ,1456 ]

part1 :: IO ()
part1 = do
  nums <- readData
  let a = map toRes $ findProd 2020 nums
  print a

part2 :: IO ()
part2 = do
  nums <- readData
  let a = sol3 nums
  print a
