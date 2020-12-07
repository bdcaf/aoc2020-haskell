module Day07 where
import System.IO
import           Text.ParserCombinators.Parsec.Token
import           Text.ParserCombinators.Parsec
import Data.Either (fromRight)
import Data.List (sort, nub)

data Bag  = Bag String
  deriving (Eq, Show, Ord)

data Contained = Contained Integer Bag
  deriving Show

type Rules = (Bag, [Contained])

getCBag (Contained _ b) = b

parseBag :: GenParser Char st Bag
parseBag = do 
  prop <- parseProp
  spaces
  string "bag"
  optional (char 's')
  return (Bag prop)

parseProp = do
  w1 <- many1 alphaNum
  spaces
  w2 <- many1 alphaNum
  return (w1 ++ " " ++ w2)

parseContBag = do
  d <- many1 digit
  spaces
  b <- parseBag
  return $ Contained (read d) b

parseLine = do
  parent <- parseBag
  spaces
  string "contain"
  spaces
  contained <- (string "no other bags" >> return []) <|> sepBy1 parseContBag (string ", ")
  char '.'
  return (parent, contained)

readInput = fromRight [] . parse (sepEndBy1 parseLine newline) "input"

readData = do
  handle   <- openFile "data/day07.txt" ReadMode
  contents <- hGetContents handle
  return $ readInput contents

example = "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
          \dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
          \bright white bags contain 1 shiny gold bag.\n\
          \muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
          \shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
          \dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
          \vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
          \faded blue bags contain no other bags.\n\
          \dotted black bags contain no other bags."

toLink (b0, []) = []
toLink (b0, b:bs) = (getCBag b,b0) : toLink (b0, bs)

linkList :: [(Bag, [Contained])] -> [(Bag,Bag)]
linkList = concatMap toLink 

findTarg b ll = nub .sort $ direct ++ indirect
  where direct = map snd . filter (\(b0,b1) -> b == b0) $ ll 
        indirect = concatMap (`findTarg` ll) direct


sol1 rules = length cands
  where ll = linkList rules
        cands = findTarg (Bag "shiny gold") ll

contBags :: Bag -> [Rules] -> Integer
contBags b rules = sum . map oneDeg $ matches
  where matches = map snd . filter (\(a,_) -> a == b) $ rules
        cont :: Contained -> Integer
        cont (Contained n b2) = n * (1 + contBags b2 rules)
        oneDeg :: [Contained] -> Integer
        oneDeg = sum . map cont


        

sol2 = contBags (Bag "shiny gold") 


part1 :: IO ()
part1 = do
  inp <- readData
  print (sol1 inp)
  

part2 :: IO ()
part2 = do
  inp <- readData
  print (sol2 inp)

