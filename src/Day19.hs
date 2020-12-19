module Day19 where

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)

readData =
  do handle <- openFile "data/Day19.txt" ReadMode
     contents <- hGetContents handle
     return $ readInput contents

--readInput :: String -> [Int]
readInput = fromRight undefined . parse pInput "input"

type RuleNum = Int

type RuleList = [RuleNum]

data Rules
  = Relative RuleList
  | Alternatives RuleList RuleList
  | OneChar Char
  deriving Show

example = readInput "0: 4 1 5\n\
          \1: 2 3 | 3 2\n\
          \2: 4 4 | 5 5\n\
          \3: 4 5 | 5 4\n\
          \4: \"a\"\n\
          \5: \"b\"\n\
          \\n\
          \ababbb\n\
          \bababa\n\
          \abbbab\n\
          \aaabbb\n\
          \aaaabbb"

ex2 = readInput "42: 9 14 | 10 1\n\
                \9: 14 27 | 1 26\n\
                \10: 23 14 | 28 1\n\
                \1: \"a\"\n\
                \11: 42 31\n\
                \5: 1 14 | 15 1\n\
                \19: 14 1 | 14 14\n\
                \12: 24 14 | 19 1\n\
                \16: 15 1 | 14 14\n\
                \31: 14 17 | 1 13\n\
                \6: 14 14 | 1 14\n\
                \2: 1 24 | 14 4\n\
                \0: 8 11\n\
                \13: 14 3 | 1 12\n\
                \15: 1 | 14\n\
                \17: 14 2 | 1 7\n\
                \23: 25 1 | 22 14\n\
                \28: 16 1\n\
                \4: 1 1\n\
                \20: 14 14 | 1 15\n\
                \3: 5 14 | 16 1\n\
                \27: 1 6 | 14 18\n\
                \14: \"b\"\n\
                \21: 14 1 | 1 14\n\
                \25: 1 1 | 1 14\n\
                \22: 14 14\n\
                \8: 42\n\
                \26: 14 22 | 1 20\n\
                \18: 15 15\n\
                \7: 14 5 | 1 21\n\
                \24: 14 1\n\
                \\n\
                \abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\n\
                \bbabbbbaabaabba\n\
                \babbbbaabbbbbabbbbbbaabaaabaaa\n\
                \aaabbbbbbaaaabaababaabababbabaaabbababababaaa\n\
                \bbbbbbbaaaabbbbaaabbabaaa\n\
                \bbbababbbbaaaaaaaabbababaaababaabab\n\
                \ababaaaaaabaaab\n\
                \ababaaaaabbbaba\n\
                \baabbaaaabbaaaababbaababb\n\
                \abbbbabbbbaaaababbbbbbaaaababb\n\
                \aaaaabbaabaaaaababaa\n\
                \aaaabbaaaabbaaa\n\
                \aaaabbaabbaaaaaaabbbabbbaaabbaabaaa\n\
                \babaaabbbaaabaababbaabababaaab\n\
                \aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"

pInt = read <$> many1 digit

pRuleLine =
  do rn <- pInt
     char ':'
     spaces
     ru <- pRule
     return (rn,ru)

pRule = try pAlternates <|> try pChar <|> pOneRL

pChar =
  do char '"'
     c <- anyChar
     char '"'
     return $ OneChar c

pOneRL = Relative <$> pRuleList
pAlternates =
  do r1 <- pRuleList
     spaces
     char '|'
     spaces
     r2 <- pRuleList
     return $ Alternatives r1 r2

pRuleList = sepEndBy1 pInt (char ' ')

pAllRules = sepEndBy1 pRuleLine newline

pStrings = sepEndBy1 (many1 alphaNum) newline

pInput = do
  ar <- pAllRules
  newline
  st <- pStrings
  return (M.fromList ar, st)

-- solve 1
m1 rl st  
  | sm == Just "" = True
  | otherwise = False
  where sm = match rl (rl M.! 0) st

match :: (M.Map Int Rules) -> Rules -> String -> Maybe String
match arl (OneChar c) [] = Nothing  
match arl (OneChar c) (s0:st)  
  | c == s0 = Just st
  | otherwise = Nothing

match arl (Relative []) st = Just st
match arl (Relative (r0:rl)) st = m0 >>= match arl (Relative rl) 
  where rr = arl M.! r0
        m0 = match arl rr st
match arl (Alternatives a1 a2) st 
  | isJust m1 = m1 
  | isJust m2 = m2 
  | otherwise = Nothing
  where m1 = match arl (Relative a1) st 
        m2 = match arl (Relative a2) st


sol1 input = length ms
  where mf = m1 . fst $input
        ms = filter mf (snd input)

-- sol2 

sol2 inp =  length ms
  where r2 = updateRule . fst $ inp
        updateRule = M.insert 8 (Alternatives [42] [42,8]) . M.insert 11 ( Alternatives [42,31] [42,11,31]) 
        mf = m2 r2
        ms = filter mf (snd inp)

m2 rl st= any (==0) . map length $ sm
  where sm = match2 rl (rl M.! 0) st

match2 :: (M.Map Int Rules) -> Rules -> String ->  [String]
match2 arl (OneChar c) [] = []  
match2 arl (OneChar c) (s0:st)  
  | c == s0 = [ st]
  | otherwise = []

match2 arl (Relative []) st =  [st]
match2 arl (Relative (r0:rl)) st = m0 >>= match2 arl (Relative rl) 
  where rr = arl M.! r0
        m0 = match2 arl rr st
match2 arl (Alternatives a1 a2) st = m1 ++ m2
  where m1 = match2 arl (Relative a1) st 
        m2 = match2 arl (Relative a2) st

part1 :: IO ()
part1 =
  do inp <- readData
     print (sol1 inp)

part2 :: IO ()
part2 =
  do inp <- readData
     print (sol2 inp)

