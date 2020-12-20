{-# LANGUAGE TupleSections #-}
module Day20 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.List (group, elemIndices)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S

example = readInput 
          "Tile 2311:\n\
          \..##.#..#.\n\
          \##..#.....\n\
          \#...##..#.\n\
          \####.#...#\n\
          \##.##.###.\n\
          \##...#.###\n\
          \.#.#.#..##\n\
          \..#....#..\n\
          \###...#.#.\n\
          \..###..###\n\
          \\n\
          \Tile 1951:\n\
          \#.##...##.\n\
          \#.####...#\n\
          \.....#..##\n\
          \#...######\n\
          \.##.#....#\n\
          \.###.#####\n\
          \###.##.##.\n\
          \.###....#.\n\
          \..#.#..#.#\n\
          \#...##.#..\n\
          \\n\
          \Tile 1171:\n\
          \####...##.\n\
          \#..##.#..#\n\
          \##.#..#.#.\n\
          \.###.####.\n\
          \..###.####\n\
          \.##....##.\n\
          \.#...####.\n\
          \#.##.####.\n\
          \####..#...\n\
          \.....##...\n\
          \\n\
          \Tile 1427:\n\
          \###.##.#..\n\
          \.#..#.##..\n\
          \.#.##.#..#\n\
          \#.#.#.##.#\n\
          \....#...##\n\
          \...##..##.\n\
          \...#.#####\n\
          \.#.####.#.\n\
          \..#..###.#\n\
          \..##.#..#.\n\
          \\n\
          \Tile 1489:\n\
          \##.#.#....\n\
          \..##...#..\n\
          \.##..##...\n\
          \..#...#...\n\
          \#####...#.\n\
          \#..#.#.#.#\n\
          \...#.#.#..\n\
          \##.#...##.\n\
          \..##.##.##\n\
          \###.##.#..\n\
          \\n\
          \Tile 2473:\n\
          \#....####.\n\
          \#..#.##...\n\
          \#.##..#...\n\
          \######.#.#\n\
          \.#...#.#.#\n\
          \.#########\n\
          \.###.#..#.\n\
          \########.#\n\
          \##...##.#.\n\
          \..###.#.#.\n\
          \\n\
          \Tile 2971:\n\
          \..#.#....#\n\
          \#...###...\n\
          \#.#.###...\n\
          \##.##..#..\n\
          \.#####..##\n\
          \.#..####.#\n\
          \#..#.#..#.\n\
          \..####.###\n\
          \..#.#.###.\n\
          \...#.#.#.#\n\
          \\n\
          \Tile 2729:\n\
          \...#.#.#.#\n\
          \####.#....\n\
          \..#.#.....\n\
          \....#..#.#\n\
          \.##..##.#.\n\
          \.#.####...\n\
          \####.#.#..\n\
          \##.####...\n\
          \##..#.##..\n\
          \#.##...##.\n\
          \\n\
          \Tile 3079:\n\
          \#.#.#####.\n\
          \.#..######\n\
          \..#.......\n\
          \######....\n\
          \####.#..#.\n\
          \.#...#.##.\n\
          \#.#####.##\n\
          \..#.###...\n\
          \..#.......\n\
          \..#.###...\n"


data Tile = Tile {tileNum::Int, tileCont:: [String]}
  deriving Show

parseTile :: GenParser Char s Tile
parseTile = do 
  string "Tile"
  spaces
  n <- read <$> many1 digit
  char ':'
  newline
  vs <- count 10 pLine
  return (Tile n vs)
pLine = do
  vs <- count 10 (char '#' <|> char '.')
  newline
  return vs

readData = do
  handle   <- openFile "data/Day20.txt" ReadMode
  contents <- hGetContents handle
  return $ readInput contents

readInput = fromRight [] . parse (sepEndBy1 parseTile newline) "input" 

topSide  = head 
bottomSide  = last 
leftSide  = map head 
rightSide  = map last 


--pickStart mm = head . head $ sv
  --where sv = filter ((==2) . length) . group . map fst . M.keys $ mm

arrStart = M.singleton (0,0)
arrAdd arr ta = undefined
surround arr = S.toList $ neigh `S.difference` M.keysSet arr
  where mk = M.keys arr
        neigh = S.fromList . concatMap directNeighbors $ mk

tileRot (Tile n ts) =   Tile n (rot ts)
rot arr = map (reverse . (\i -> map ( !! i) arr)) [0..((length . head $ arr)-1)]
tileFlipLR (Tile n ts) = Tile n (map reverse ts)
tileFlipUD (Tile n ts) = Tile n (reverse ts)

allOrientations = concatMap allRots . allFlips 
  where allFlips t1 = [t1 , tileFlipLR t1] -- flip ud already covered by rotation
        allRots  = take 4 . iterate tileRot

grow1 arr [] = [arr]
grow1 arr ts = concatMap (uncurry grow1) goodAr2
  where cands = surround arr
        ar2s = tryCand arr cands ts
        goodAr2 = filter ((< length ts) . length . snd) ar2s
tryCand arr [] ts = [(arr, ts)]
tryCand arr (c0:cs) ts = concatMap (\(a,t) -> tryCand a cs t ) r1
  where r1 = tryTile arr c0 ts []

tryTile arr cand [] tr = [(arr, tr)]
tryTile arr cand (t:ts) tr = r2  ++ tryTile arr cand ts (t:tr)
  where te = tryExplicit arr cand t
        a2 = map (\t -> M.insert cand t arr) te
        r2 = map (, tr ++ ts) a2
tryExplicit arr cand t = filter (doesFit arr cand) ts
  where ts = allOrientations t 
doesFit arr (xc,yc) (Tile _ t1) = and $ catMaybes [ft,fb,fl,fr]
  where
        fitTop  tf = last tf == head t1
        fitBottom tf = head tf == last t1
        fitLeft tf = map last tf == map head t1
        fitRight tf = map head tf == map last t1
        ft = fitTop . tileCont <$> arr M.!? (xc,yc+1)
        fb = fitBottom . tileCont <$> arr M.!? (xc,yc-1)
        fl = fitLeft . tileCont <$> arr M.!? (xc-1,yc)
        fr = fitRight . tileCont <$> arr M.!? (xc+1,yc)


directNeighbors (x,y) = [(x+1,y), (x-1,y), (x,y-1), (x,y+1)]

arrange matched [] = matched
arrange matched (u:us) = undefined
  where k1 = M.keys matched

extractCornerId ma = mapMaybe (`M.lookup` ma) pl
  where ks = M.keys ma
        xs = map fst ks
        ys = map snd ks
        xr = [minimum xs, maximum xs]
        yr = [minimum ys, maximum ys]
        pl = [(x,y) | x <- xr, y<-yr]
resVal cr
  | length cr == 4 = Just . product . map tileNum $ cr
  | otherwise = Nothing


sol1 inp = r1
  where 
    a1 = arrStart . head $ inp
    rls = grow1 a1 (tail inp)
    rvs = mapMaybe (resVal . extractCornerId) rls
    r1 = head rvs
--sol1 inp = mm
  --where 
    --tr = map toTileRed inp
    --mm = buildMap tr
    --sv = pickStart mm

-- something went wrong with y - need the reverses to reproduce example
toBM ma = reverse $ jRows rows
  where ks = M.keys ma
        xs = map fst ks
        ys = map snd ks
        xr = [minimum xs .. maximum xs]
        yr = reverse [minimum ys .. maximum ys]
        xkeys x = [(x,y) | y <- yr] 
        colTiles x = concatMap (remBorder.tileCont . (ma M.!)) . xkeys $ x
        rows = map colTiles xr
        jRows [x] = x
        jRows (a:b:as) = jRows (zipWith (++) a b :as)
remBorder =  map (init . tail) . init . tail 

monster = "                  # \n\
          \#    ##    ##    ###\n\
          \ #  #  #  #  #  #   "
toPat = S.fromList . concat . zipWith (\a l -> map (,a) l) [0..] . map pl 
  where pl = elemIndices '#' 

mp = toPat . lines $ monster

findRange pat = ( maximum xs, maximum ys)
  where 
    pl = S.toList pat
    xs = map fst pl
    ys = map snd pl

shiftPat (xs,ys) = map (\(x,y) -> (x+xs, y+ys)) 

findPat imgPat searchPat = (length sps, S.size diffP)
  where (ixm, iym) = findRange imgPat
        (ixs, iys) = findRange searchPat
        shifts = [(x,y) | x <- [0..(ixm - ixs)],
                          y <- [0..(iym - iys)]]
        exactMatch [] = True
        exactMatch (s:ss)
          | s `elem` imgPat = exactMatch ss
          | otherwise = False
        sps =  filter exactMatch .  map (`shiftPat` S.toList searchPat) $ shifts
        diffP = imgPat `S.difference` S.fromList (concat sps)
  

allArrangements p = concatMap rots [p, reverse p]
  where rots = take 4 . iterate rot


sol2 inp = filter ((>0) . fst) r2
  where 
    a1 = arrStart . head $ inp
    rls = grow1 a1 (tail inp)
    img = head rls
    bm = toBM img
    rs = allArrangements bm
    r2 = map ((`findPat` mp) . toPat) rs
    bp = toPat bm
    res = findPat bp mp



part1 :: IO ()
part1 = do
  inp <- readData
  print (sol1 inp)
  

part2 :: IO ()
part2 = do
  inp <- readData
  print (sol2 inp)

