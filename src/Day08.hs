module Day08 where

import Prelude as P
import System.IO
import Control.Monad.State
import Data.Vector hiding (modify,  (++))
import Control.Monad (when)
import           Text.ParserCombinators.Parsec.Token
import           Text.ParserCombinators.Parsec hiding (State)
import Data.Either (fromRight, isRight)

newtype Argument = Argument Int
  deriving Show
data Operation = Acc | Jmp | Nop
  deriving (Show, Eq)
data Instruction = Instruction Operation Argument 
  deriving Show

data Program = Program Int (Vector Instruction) (Vector Int)
  deriving Show
toProg lp  = Program 0 (fromList lp) (Data.Vector.replicate (P.length lp) 0)

type GlobalValue = Int
type Machine a = StateT Program (State GlobalValue ) a

fromArgument (Argument a) = a

progStep  = progJump 1
progJump d (Program c b l) = Program (c+d) b l
hasConcluded (Program c b l) = c >= Data.Vector.length b

currExp (Program c b l) = (b!c, l!c)
markExec (Program c b l) = Program c b (l // [(c, 1 + l!c)])


run1state :: Machine ()
run1state = do
  --printLine
  (ins, count) <- gets currExp
  when (count == 0) $ do
       modify markExec
       execute ins
       run1state
  
run2state :: Machine (Either Int Int)
run2state = do
  done <- gets hasConcluded
  if done 
     then do
      a <- lift get
      return (Right a)
     else do
      --printLine
      (ins, count) <- gets currExp
      if count == 0 
         then do
           modify markExec
           execute ins
           run2state
         else do
          a <- lift get
          return (Left a)


execute :: Instruction -> Machine ()
execute (Instruction Acc a) = do
  lift  (modify (+ fromArgument a) )
  modify progStep
execute (Instruction Nop a) = modify progStep
execute (Instruction Jmp a) = modify (progJump (fromArgument a))


oneFlip p = flip1 [] p
  where 
    flip1 pre [] = []
    flip1 pre (a0@(Instruction Acc b):post) = 
          flip1 (pre++[a0]) post
    flip1 pre (a0@(Instruction Nop b):post) = 
      (pre++(Instruction Jmp b):post) :
          flip1 (pre++[a0]) post
    flip1 pre (a0@(Instruction Jmp b):post) = 
      (pre++(Instruction Nop b):post) :
          flip1 (pre++[a0]) post

run1 p0 = execState (execStateT run1state p0) 0 
run2 p0 = evalState (evalStateT run2state p0) 0 

sol1 = run1 . toProg

sol2 p = P.filter isRight res
  where pl = oneFlip p
        res = P.map (run2 . toProg) pl

part1 = do
  d <- readData
  print (sol1 d) 
part2 = do
  d <- readData
  print (sol2 d) 

example = "nop +0\n\
          \acc +1\n\
          \jmp +4\n\
          \acc +3\n\
          \jmp -3\n\
          \acc -99\n\
          \acc +1\n\
          \jmp -4\n\
          \acc +6"

p0 = parseProgram example


parseArg :: GenParser Char s Argument 
parseArg = do
  s <- oneOf "+-"
  a <- read <$> many1 digit
  let num = if s == '-' then -a
                        else a
  return (Argument num)
parseOp = try (string "nop" >> return Nop) <|>
  try (string "acc" >> return Acc) <|>
  (string "jmp" >> return Jmp)

parseIns :: GenParser Char s Instruction
parseIns = do
  o <- parseOp
  spaces
  Instruction o <$> parseArg

parseProgram = fromRight [] . parse  (sepEndBy1 parseIns newline) "prog"

readData = do
  handle   <- openFile "data/day08.txt" ReadMode
  contents <- hGetContents handle
  return $ parseProgram contents

