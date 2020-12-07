module Day02 where
import System.IO
import           Text.ParserCombinators.Parsec.Token
import           Text.ParserCombinators.Parsec
import Data.Either (fromRight)
import Data.Algebra.Boolean (xor)
import Data.Either(isRight, isLeft)

type Key = String
type Value = String
type Field = (String, String)
type Passport = [Field]

example = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
          \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
          \\n\
          \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
          \hcl:#cfa07d byr:1929\n\
          \\n\
          \hcl:#ae17e1 iyr:2013\n\
          \eyr:2024\n\
          \ecl:brn pid:760753108 byr:1931\n\
          \hgt:179cm\n\
          \\n\
          \hcl:#cfa07d eyr:2025 pid:166559648\n\
          \iyr:2011 ecl:brn hgt:59in"

required :: [Key]
required = [ "byr"
           , "iyr"
           , "eyr"
           , "hgt"
           , "hcl"
           , "ecl"
           , "pid"
            ]

optional = ["cid"]

testData = fromRight [] $ parse parseInput "read data"  example
readData = do
  handle   <- openFile "data/day04.txt" ReadMode
  contents <- hGetContents handle
  return $ fromRight [] $ parse parseInput "read data"  contents

parseInt :: GenParser Char st Int
parseInt = read <$> many1 digit

parseKey :: GenParser Char st Key
parseKey = count 3 alphaNum
parseValue :: GenParser Char st Key
parseValue = many1 (noneOf " \n")

parseField :: GenParser Char st Field
parseField = do
  k <- parseKey
  char ':'
  v <- parseValue
  return (k,v)

separator = newline <|> space
parseEntry :: GenParser Char st Passport
parseEntry = sepEndBy1 parseField separator

parseInput :: GenParser Char st [Passport]
parseInput = sepBy1 parseEntry newline

data Hgt = HCm Int | HIn Int deriving Show
parseHgt = do 
    v <- pint
    cons <- parseCm <|> parseIn 
    return (cons v)
  where parseCm = do
          string "cm"
          return HCm
        parseIn = do
          string "in"
          return HIn
        pint :: GenParser Char st Int
        pint = read <$> many1 digit

newtype Hcl = Hcl String
  deriving Show
parseHcl :: GenParser Char st Hcl
parseHcl = do
  char '#'
  val <- count 6 (oneOf "0123456789abcdef")
  eof
  return (Hcl val)
newtype Ecl = Ecl String
  deriving Show
parseEcl = try (string "amb" )
  <|> try (string "blu") 
  <|> try (string "brn")
  <|> try ( string "gry" )
  <|> try (string "grn")
  <|> try (string "hzl")
  <|> string "oth"

newtype Pid = Pid String
  deriving Show
parsePid = do
  pid <- count 9 digit
  eof
  return (Pid pid)
isValid :: Passport -> Bool
isValid p = all (`elem` fns) required
  where fns = map fst p

  
sol1 = length . filter isValid 

getField fn p = snd . head $ f1
  where f1 = filter (\f -> fst f == fn) p

checkInt fn lo hi p = lo <= fv && fv <= hi
  where fv = read (getField fn p)

checkParse :: String -> GenParser Char () v -> Passport -> Bool
checkParse fd parser p = isRight pv
  where he = getField fd p
        pv = parse parser fd he



checkH p = checkP hv
  where he = getField "hgt" p
        hv = parse parseHgt "hgt" he
        checkP (Left _) = False
        checkP (Right b) = check b
        check (HIn a) = 56<= a && a <= 76
        check (HCm a) = 150<= a && a <= 193

isValid2 p = byr p && iyr p && eyr p && checkH p 
                && hcl p
                && ecl p
                && pid p

byr = checkInt "byr" 1920 2002
iyr = checkInt "iyr" 2010 2020
eyr = checkInt "eyr" 2020 2030
hcl = checkParse "hcl" parseHcl 
ecl = checkParse "ecl" parseEcl 
pid = checkParse "pid" parsePid

sol2 = length . filter isValid2 . filter isValid 

part1 :: IO ()
part1 = do
  inp <- readData
  let res = sol1 inp
  print res

part2 :: IO ()
part2 = do
  inp <- readData
  let res = sol2 inp
  print res

testByr = byr [("byr","2002")] 
  && not (byr [("byr","2003")])

testHgt = checkH [("hgt","60in")] 
  && checkH [("hgt","190cm")]
  && not (checkH [("hgt","190in")])
  && not (checkH [("hgt","190")])

testHcl = hcl [("hcl","#123abd")] 
  && not (hcl [("hcl","#123abz")])
  && not (hcl [("hcl","123abc")])

testEcl = ecl [("ecl","brn")]
  && not (ecl [("ecl","wat")])

