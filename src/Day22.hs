{-# LANGUAGE BangPatterns #-}

module Day22 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import qualified Data.Set as S

readData =
  do handle <- openFile "data/Day22.txt" ReadMode
     contents <- hGetContents handle
     return $ readInput contents

readInput = fromRight undefined . parse (sepEndBy1 pPlayer newline) "input"

pPlayer =
  do string "Player"
     spaces
     id <- read <$> many1 digit
     char ':'
     newline
     cards <- sepEndBy1 pCard newline
     return $ Player id cards

pCard = Card . read <$> many1 digit

example =
  readInput "Player 1:\n9\n2\n6\n3\n1\n\n\
  \Player 2:\n5\n8\n4\n7\n10\n"

data Player =
  Player {pId :: Int
         ,pCards :: [Card]}
  deriving (Show,Eq,Ord)

newtype Card = Card {unCard :: Int}
  deriving (Eq,Ord,Show)

gameRound pb (Player a []) = pb
gameRound (Player a []) pb = pb
gameRound (Player a (ca:cas)) (Player b (cb:cbs))
  | ca > cb = gameRound (Player a (cas ++ [ca,cb])) (Player b cbs)
  | otherwise = gameRound (Player a cas) (Player b (cbs ++ [cb,ca]))

gameScore cards = sum . zipWith (\a b -> a * unCard b) [1 ..] $ reverse cards

sol1 inp = gameScore $ pCards $ gameRound (head inp) (inp !! 1)

-- needed lots of debugging -- error was the condition for recursiveCombat
type Memory = S.Set (Player,Player)

data Game2
  = Winner Player
  | Running Player Player
  deriving Show

data GameState =
  GameState {gMem :: Memory
            ,gCur :: Game2}
  deriving Show

--for debugging
doRound2 s@(GameState m (Winner a)) = s
doRound2 s@(GameState m (Running pa pb)) = gameRound2 m pa pb

gameRound2 !m (Player _ []) pa = GameState m $ Winner pa
gameRound2 !m pa (Player _ []) = GameState m $ Winner pa
gameRound2 !gm pa@(Player a (ca:cas)) pb@(Player b (cb:cbs))
  | S.member (pa,pb) gm = GameState gm $ Winner pa
  | unCard ca <= length cas && unCard cb <= length cbs =
    case pId rc of
      1 -> winA
      _ -> winB
  | ca > cb = winA
  | ca < cb = winB
  where g2 = S.insert (pa,pb) gm

        rc =
          recursiveCombat (Player a (take (unCard ca) cas))
                          (Player b (take (unCard cb) cbs))

        winA =
          GameState g2 $ Running (Player a (cas ++ [ca,cb])) (Player b cbs)

        winB =
          GameState g2 $ Running (Player a cas) (Player b (cbs ++ [cb,ca]))

recursiveCombat pa pb = runRec (GameState S.empty (Running pa pb))

runRec (GameState !mem (Winner !a)) = a
runRec (GameState !mem (Running !a !b)) = runRec (gameRound2 mem a b)

initGame ps = GameState S.empty (Running (head ps) (ps !! 1))

sol2 ps = gameScore . pCards $ recursiveCombat (head ps) (ps !! 1)

part1 :: IO ()
part1 =
  do inp <- readData
     print (sol1 inp)

part2 :: IO ()
part2 =
  do inp <- readData
     print (sol2 inp)

