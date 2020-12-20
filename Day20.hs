import Debug.Trace (trace)
import Data.List (foldl', nub)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Edged a = Edged !a !([a]) !a deriving (Show, Eq, Ord)

type Tile = Edged (Edged Char)
type Edge = Edged Char
type Edges a = (Edged a, Edged a, Edged a, Edged a)
type Input = [(Int, Tile)]

main :: IO ()
main = print . (solve =<<) . decode =<< readFile "Day20.input"

solve :: Input -> Maybe (Int, Int)
solve ts = do
  let em = edgeMap ts
  (c, cs) <- borders em
  let answer1 = product . map fst $ c : cs
  _ <- build c em
  pure (answer1, 0)

build :: (Int, Tile) -> Map Edge [(Int, Tile)] -> Maybe [Tile]
build (_, e) em = do
  c <- traverse check (edges e) >>= \case
    [True, False, False, True] -> pure e
    [True, True, False, False] -> pure $ mapEdged rotate e
    [False, True, True, False] -> pure $ mapEdged (rotate . rotate) e
    [False, False, True, True] -> pure $ mapEdged (rotate . rotate . rotate) e
    _                          -> Nothing
  [_, r, _, _] <- pure $ edges c
  ts <- Map.lookup r em
  pure $ c : map snd ts
  where
    check x = (== 1) . length <$> Map.lookup x em

borders :: Ord a => Map Edge [a] -> Maybe (a, [a])
borders em = do
  let xs = Map.toList $ foldl' count Map.empty [ t | (_, [t]) <- Map.toList em ]
  (c1 : cs@[_, _, _]) <- pure [ t | (t, n) <- xs, n == 4 ]
  pure (c1, cs)
  where
    count = flip $ Map.insertWith (+) `flip` (1 :: Int)

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

edges :: Edged (Edged a) -> [Edged a]
edges (Edged f@(Edged ff _ fl) xs l@(Edged lf _ ll)) =
  [ f
  , Edged fl (map (\(Edged _ _ x) -> x) xs) ll
  , l
  , Edged ff (map (\(Edged x _ _) -> x) xs) lf
  ]

allEdges :: Tile -> [Edge]
allEdges t = nub $ concatMap edges
  [ t
  , mapEdged reverse t
  , mapEdged (map reverse) t
  ]

rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

transpose :: [[a]] -> [[a]]
transpose vs = case fmap unzip $ traverse uncons vs of
  Just (a, as) -> a : transpose as
  Nothing      -> []
  where
    uncons []       = Nothing
    uncons (x : xs) = Just (x, xs)

mapEdged :: ([[a]] -> [[a]]) -> Edged (Edged a) -> Edged (Edged a)
mapEdged f =
  fromJust . (fromList =<<) . traverse fromList . f . map toList . toList

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
