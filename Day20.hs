import Control.Monad (guard)
import Data.List (foldl', nub)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Edged a = Edged !a !([a]) !a deriving (Show, Eq, Ord)

type Tile = Edged (Edged Char)
type Edge = Edged (Char)
type Edges = (Edge, Edge, Edge, Edge)
type Input = [(Int, Tile)]

main :: IO ()
main = print . (solve =<<) . decode =<< readFile "Day20.input"

solve :: Input -> Maybe (Int, Int)
solve ts = do
  let em = edgeMap ts
  let cornerIds = [ i | (i, n) <- Map.toList (idsToNums em), n == 4 ]
  guard $ length cornerIds == 4
  pure (product cornerIds, 0)

idsToNums :: Map Edge [(Int, Tile)] -> Map Int Int
idsToNums em = foldl' f Map.empty [ i | (_, [(i, _)]) <- Map.toList em ]
  where
    f = flip $ Map.insertWith (+) `flip` (1 :: Int)

edgeMap :: Input -> Map Edge [(Int, Tile)]
edgeMap = foldl' step Map.empty
  where
    step m (i, t) = foldl' append m $ allEdges t
      where
        append = flip $ Map.insertWith (<>) `flip` [(i, t)]

decode :: String -> Maybe Input
decode = parseMaybe (many $ tileP <* optional newline)

tileP :: Parser (Int, Tile)
tileP = do
  i <- string "Tile " *> decimal <* string ":" <* newline
  t <- edgedP (edgedP (char '.' <|> char '#') <* newline)
  pure (i, t)

edgedP :: Parser a -> Parser (Edged a)
edgedP p = do
  xs <- many p
  Just e <- pure (fromList xs) <?> "Have enough elements"
  pure e

edges :: Tile -> Edges
edges (Edged f@(Edged ff _ fl) xs l@(Edged lf _ ll)) =
  ( f
  , Edged fl (map (\(Edged _ _ x) -> x) xs) ll
  , reverseEdged $ l
  , reverseEdged $ Edged ff (map (\(Edged x _ _) -> x) xs) lf
  )

allEdges :: Tile -> [Edge]
allEdges t = nub $ concatMap fromEdges [es, flipH es, flipV es]
  where
    es = edges t
    fromEdges (u, r, d, l) = [u, r, d, l]

rotate, flipV, flipH :: Edges -> Edges
rotate (u, r, d, l) = (l, u, r, d)
flipV (u, r, d, l) = (d, reverseEdged r, u, reverseEdged l)
flipH (u, r, d, l) = (reverseEdged u, l, reverseEdged d, r)

toList :: Edged a -> [a]
toList (Edged x xs y) = x : xs ++ [y]

fromList :: [a] -> Maybe (Edged a)
fromList []     = Nothing
fromList (x:xs) =
  case reverse xs of
    []    -> Nothing
    (y:_) -> Just $ Edged x (init xs) y

reverseEdged :: Edged a -> Edged a
reverseEdged (Edged f xs l) = Edged l (reverse xs) f
