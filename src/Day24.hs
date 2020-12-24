module Day24 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import Data.List (sort,group)
import qualified Data.Set as S

example =
  readInput "sesenwnenenewseeswwswswwnenewsewsw\n\
  \neeenesenwnwwswnenewnwwsewnenwseswesw\n\
  \seswneswswsenwwnwse\n\
  \nwnwneseeswswnenewneswwnewseswneseene\n\
  \swweswneswnenwsewnwneneseenw\n\
  \eesenwseswswnenwswnwnwsewwnwsene\n\
  \sewnenenenesenwsewnenwwwse\n\
  \wenwwweseeeweswwwnwwe\n\
  \wsweesenenewnwwnwsenewsenwwsesesenwne\n\
  \neeswseenwwswnwswswnw\n\
  \nenwswwsewswnenenewsenwsenwnesesenew\n\
  \enewnwewneswsewnwswenweswnenwsenwsw\n\
  \sweneswneswneneenwnewenewwneswswnese\n\
  \swwesenesewenwneswnwwneseswwne\n\
  \enesenwswwswneneswsenwnewswseenwsese\n\
  \wnwnesenesenenwwnenwsewesewsesesew\n\
  \nenewswnwewswnenesenwnesewesw\n\
  \eneswnwswnwsenenwnwnwwseeswneewsenese\n\
  \neswnwewnwnwseenwseesewsenwsweewe\n\
  \wseweeenwnesenwwwswnew"

readData =
  do handle <- openFile "data/Day24.txt" ReadMode
     contents <- hGetContents handle
     return $ readInput contents

readInput = fromRight [] . parse pInput "input"

data Directions
  = East
  | SouthEast
  | SouthWest
  | West
  | NorthWest
  | NorthEast
  deriving (Show,Eq,Enum)

pInput = sepEndBy1 pPath newline

pPath = many1 pDir

pDir = try se <|> try sw <|> try nw <|> try ne <|> e <|> w
  where e = char 'e' >> return East

        se = string "se" >> return SouthEast

        sw = string "sw" >> return SouthWest

        w = string "w" >> return West

        ne = string "ne" >> return NorthEast

        nw = string "nw" >> return NorthWest

--  /
-- /---
walk [] ref = ref
walk (p:ps) ref = walk ps (step p ref)

step East (x,y) = (x + 1,y)
step SouthEast (x,y) = (x + 1,y - 1)
step SouthWest (x,y) = (x,y - 1)
step West (x,y) = (x - 1,y)
step NorthEast (x,y) = (x,y + 1)
step NorthWest (x,y) = (x - 1,y + 1)

sol1 = S.size . toGrid

neighbors (x,y) = map (`step` (x,y)) [East .. NorthEast]

toGrid inp = S.fromList flipped
  where poss = map (`walk` (0,0)) inp

        flipped =
          map head . filter ((== 1) . (`mod` 2) . length) . group . sort $ poss

growth grid = S.union (S.filter blackFilt grid) (S.filter whiteFilt surr)
  where surr =
          (S.fromList . concatMap neighbors . S.toList $ grid)
          `S.difference` grid

        blackNeigh tile = ln
          where cands = neighbors tile

                neigh = S.intersection (S.fromList cands) grid

                ln = S.size neigh

        blackFilt tile
          | 0 < n && n < 3 = True
          | otherwise = False
          where n = blackNeigh tile

        whiteFilt tile
          | n == 2 = True
          | otherwise = False
          where n = blackNeigh tile

sol2 input = S.size $ sols !! 100
  where grid = toGrid input

        sols = iterate growth grid

part1 :: IO ()
part1 =
  do inp <- readData
     print (sol1 inp)

part2 :: IO ()
part2 =
  do inp <- readData
     print (sol2 inp)

