module Day02 where
import System.IO
import           Text.ParserCombinators.Parsec.Token
import           Text.ParserCombinators.Parsec
import Data.Either (fromRight)

type Answer = String

addAnswers a [] = a 
addAnswers a (n:ns) = if n `elem` a 
                         then addAnswers a ns
                         else addAnswers (n:a) ns

addAnswerB a [] = []
addAnswerB a (n:ns) = if n `elem` a 
                         then n:addAnswerB a ns
                         else addAnswerB a ns

collapse :: [Answer] -> Answer
collapse [] = []
collapse [x] = x
collapse (x:xs) = addAnswers x (collapse xs)

collapsB :: [Answer] -> Answer
collapsB [] = []
collapsB [x] = x
collapsB (x:xs) = addAnswerB x (collapsB xs)

partition :: [Bool] -> Int
partition = parts 0 
  where 
    parts :: Int -> [Bool] -> Int
    parts d [] = d
    parts d ( b:bs ) = parts (2*d + if b then 1 else 0) bs

parseAnswer = many1 alphaNum
parseGroup = sepEndBy1 parseAnswer newline
parseInput = fromRight [] . parse (sepEndBy1 parseGroup newline) "parse input"


readData = do
  handle   <- openFile "data/day06.txt" ReadMode
  contents <- hGetContents handle
  return $ parseInput contents


example = "abc\n\
          \\n\
          \a\n\
          \b\n\
          \c\n\
          \\n\
          \ab\n\
          \ac\n\
          \\n\
          \a\n\
          \a\n\
          \a\n\
          \a\n\
          \\n\
          \b"


sol1 = sum . map (length . collapse)

sol2 = sum . map (length . collapsB)


part1 :: IO ()
part1 = do
  inp <- readData
  print (sol1 inp)
  

part2 :: IO ()
part2 = do
  inp <- readData
  print (sol2 inp)

