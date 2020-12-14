module Day14 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import Data.Map.Strict as M
import Control.Monad.State.Strict as S

readData = do
  handle   <- openFile "data/Day14.txt" ReadMode
  contents <- hGetContents handle
  return $ readInput contents

example = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
          \mem[8] = 11\n\
          \mem[7] = 101\n\
          \mem[8] = 0"

readInput :: String -> [Command]
readInput = fromRight [] . parse parseProg "input" 

parseProg :: GenParser Char s [Command]
parseProg = sepEndBy1 parseLine newline
  where
    parseLine = try parseMaskCommand <|> parseMemCommand

    parseMaskCommand :: GenParser Char s Command
    parseMaskCommand = do
      string "mask"
      spaces
      char '='
      spaces
      SetMask <$> parseMask

    parseMemCommand :: GenParser Char s Command
    parseMemCommand = do
      string "mem"
      char '['
      pos <- read <$> many1 digit
      char ']'
      spaces
      char '='
      spaces
      val <- read <$> many1 digit
      return (SetAddr pos val)

    parseMask :: GenParser Char s Mask
    parseMask = Mask <$> many1 (pNot <|> pT <|> pf)
      where pNot = char 'X' >> return Nothing
            pT = char '1' >> return  (Just True)
            pf = char '0' >> return  (Just False)

data Setter = Setter Int Int
  deriving Show
newtype Mask = Mask [Maybe Bool]
  deriving Show

data Command = SetMask Mask | SetAddr Int Int
  deriving Show

type Bits = [Bool]
data Machine = Machine {mMask::Mask, mem::Map Int Int}
  deriving Show

initMachine = Machine (Mask $ replicate 36 Nothing) empty

execute :: Command -> State Machine ()
execute (SetMask m) = modify (\ma -> ma {mMask = m})
execute (SetAddr add val) = modify (machset add val)
  where
    machset add val mach@(Machine mask mem) = Machine mask (insert add bmask mem)
      where bval = toFullBits val
            bmask = fromBits $ combine mask bval
    combine _ [] = []
    combine (Mask (m:ms)) (b:bs) =
      case m of
        Nothing -> b: combine (Mask ms) bs
        Just a -> a : combine (Mask ms) bs

execute2 :: Command -> State Machine ()
execute2 (SetMask m) = modify (\ma -> ma {mMask = m})
execute2 (SetAddr add val) = modify (machset add val)
  where
    machset add val mach@(Machine mask mem) = Machine mask (multiAdd numAdd bval mem)
      where bval =  val
            addresses = combineAdd [] mask (toFullBits add)
            numAdd = Prelude.map fromBits addresses
combineAdd c _ [] = [c]
combineAdd c (Mask (m:ms)) (b:bs) =
  case m of
    Nothing -> combineAdd ( c ++ [False]) (Mask ms) bs ++
      combineAdd ( c ++ [True]) (Mask ms) bs
    Just False -> combineAdd (c ++ [b]) (Mask ms) bs
    Just True -> combineAdd (c ++ [True]) (Mask ms) bs
multiAdd [] _ mem = mem
multiAdd (a:as) b mem = multiAdd as b (insert a b mem) 


--runProg = Prelude.foldr ((>>) . execute) (return ()) 
runProg :: (Command -> State Machine ()) -> [Command] -> State Machine ()
runProg _ [] = return ()
runProg ex (p:ps) = do
  ex p 
  runProg ex ps

execProg :: (Command -> State Machine ()) -> [Command] -> Machine
execProg ex ps = execState (runProg ex ps) initMachine
  

toFullBits n = (replicate (36 - length b0) False) ++ b0
  where b0 = toBits n


toBits 0 = []
toBits n = toBits q ++ [r == 1]
  where (q,r) = divMod n 2


fromBits :: [Bool] -> Int
fromBits bs = go 0 bs
  where
    go c [] = c
    go c (True:xs) = go (2*c + 1) xs
    go c (False:xs) = go (2*c) xs


sol1 prog = res
  where ap = execProg execute prog
        m = mem ap
        vals = elems m
        res = sum vals

ex2 = readInput "mask = 000000000000000000000000000000X1001X\n\
                \mem[42] = 100\n\
                \mask = 00000000000000000000000000000000X0XX\n\
                \mem[26] = 1"
sol2 prog = res
  where ap = execProg execute2 prog
        m = mem ap
        vals = elems m
        res = sum vals


part1 :: IO ()
part1 = do
  inp <- readData
  print (sol1 inp)
  

part2 :: IO ()
part2 = do
  inp <- readData
  print (sol2 inp)

