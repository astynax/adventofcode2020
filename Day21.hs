import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Void (Void)
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char

type Parser = Parsec Void String

type Ingt = String
type Algn = String
type Food = (Set Ingt, Set Algn)

main :: IO ()
main = print . (solve =<<) . decode =<< readFile "Day21.input"

solve :: [Food] -> Maybe (Int, Int)
solve fs = do
  let im = collect fs
  -- ias <- associate im
  pure (0, 0)

associate :: Map Ingt (Map Algn Int) -> [[(Ingt, Maybe Algn)]]
associate = undefined

count :: [Food] -> Map Ingt Int
count = foldl' storeIngt Map.empty . map fst
  where
    storeIngt = foldl' (flip $ Map.insertWith (+) `flip` 1)

collect :: [Food] -> Map Ingt (Map Algn Int)
collect = foldl' storeFood Map.empty
  where
    storeFood m (is, as) = foldl' (storeIngt as) m is
    storeIngt as m i = foldl' (storeAlgn i) m as
    storeAlgn i m a = Map.insertWith (Map.unionWith (+)) i (Map.singleton a 1) m

decode :: String -> Maybe [Food]
decode = traverse (parseMaybe foodP) . lines

foodP :: Parser Food
foodP = do
  is <- some (wordP <* spaceChar)
  () <$ string "(contains "
  as <- some letterChar `sepBy` string ", "
  () <$ char ')'
  pure (Set.fromList is, Set.fromList as)
  where
    wordP = some letterChar
