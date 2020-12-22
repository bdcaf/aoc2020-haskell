module Day21 where
  -- Note:
  --  Each allergen is found in exactly one ingredient.

import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.List (groupBy, group, sort, maximumBy, minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as S

readData = do
  handle   <- openFile "data/Day21.txt" ReadMode
  contents <- hGetContents handle
  return $ readInput contents


example = readInput 
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
  \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
  \sqjhc fvjkl (contains soy)\n\
  \sqjhc mxmxvkd sbzzf (contains fish)"

pWord = many1 letter
pLine = do
  ing <- sepEndBy1 pIngredient (char ' ')
  allerg <- pAllergen
  return (ing, allerg)
pIngredient = Ingredient <$> pWord
pAllergen = do
  char '('
  string "contains"
  spaces
  all <- sepBy1 (Allergen <$> pWord) (string ", ")
  char ')'
  return all
pInput = sepEndBy1 pLine newline

readInput = fromRight undefined . parse pInput "input" 

newtype Ingredient = Ingredient String deriving (Show,Eq,Ord)
newtype Allergen = Allergen String deriving (Show,Eq,Ord)


toAllergAssoc = cleanAm M.empty . allergenMap M.empty
  where
    allergenMap map [] = map
    allergenMap map ((ings, alls):ils) = allergenMap mi2 ils
      where iv = S.fromList ings
            mi  k vals m0 = case kv of
                              Nothing -> M.insert k vals m0
                              Just xs -> M.insert k (S.intersection vals xs) m0
                              where kv = m0 M.!? k
            mi2 = foldr (`mi` iv)   map alls
    cleanAm rm map 
      | M.null map = rm
      | otherwise = cleanAm r2 badM
      where goodM = M.map (head . S.toList) . M.filter ((==1) . S.size) $  map
            r2 = M.union rm goodM
            toRemove = S.fromList . M.elems $ goodM
            badM = M.map (`S.difference` toRemove)  $ M.difference map goodM

sol1 input =   length occurrenc
  where allIng = S.fromList . concatMap fst $ input 
        fullIng = concatMap fst input
        occurrenc = filter (`S.member` resList) fullIng
        allMap = toAllergAssoc input
        resList = allIng `S.difference`  (S.fromList . M.elems $ allMap)

prettyIngred (Ingredient s) = s
toIngStr [a] = a
toIngStr (a:as) = a ++ "," ++ toIngStr as 
sol2 input =  ingList
  where allMap = toAllergAssoc input
        rl = M.toAscList allMap
        ingList = toIngStr . map ( prettyIngred . snd)  $ rl


part1 :: IO ()
part1 = do
  inp <- readData
  print (sol1 inp)
  

part2 :: IO ()
part2 = do
  inp <- readData
  print (sol2 inp)

