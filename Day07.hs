import Data.Void
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void String

type Bag = (String, String)
type Rules = Map Bag [(Int, Bag)]

goldenBag :: Bag
goldenBag = ("shiny", "gold")

main :: IO ()
main = do
  print . (== (4, 32)) . solve . decode $
    [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
    , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
    , "bright white bags contain 1 shiny gold bag."
    , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
    , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
    , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
    , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
    , "faded blue bags contain no other bags."
    , "dotted black bags contain no other bags."
    ]
  print . solve . decode . lines =<< readFile "Day07.input"

solve :: Rules -> (Int, Int)
solve rules = (containGoldenBag, containInGoldenBag)
  where
    containGoldenBag =
      subtract 1 -- without the golden bag itself
      . length . fst
      $ foldl' (findGoldenBag rules)
      (Set.singleton goldenBag, Set.empty)
      (Map.keys rules)

    containInGoldenBag = subtract 1 $ countSubbags rules goldenBag

findGoldenBag :: Rules -> (Set Bag, Set Bag) -> Bag -> (Set Bag, Set Bag)
findGoldenBag rules = go Set.empty
  where
    go visited (good, bad) bag
      | Set.member bag visited = error $ "Cycle: " ++ show (bag, visited)
      | Set.member bag bad     = (good, Set.union visited bad)
      | Set.member bag good    = (Set.union visited good, bad)
      | otherwise              =
        case Map.lookup bag rules of
          Nothing -> error "Malformed rules!"
          Just xs ->
            let visited' = Set.insert bag visited
            in foldl' (go visited') (good, bad) $ map snd xs

countSubbags :: Rules -> Bag -> Int
countSubbags rules = fst . go Map.empty
  where
    go cache bag =
      case (Map.lookup bag cache, Map.lookup bag rules) of
        (Just x, _      ) -> (x, cache)
        (_,      Just xs) ->
          let
            (v, cache') =
              foldl' (\(s, c) (n, sb)->
                       let (v', c') = go c sb
                       in (s + n * v', c'))
              (1, cache)
              xs
          in (v, Map.insert bag v cache')
        _                 -> error $ "No rules for " ++ show bag

decode :: [String] -> Rules
decode = Map.fromList . fromMaybe [] . sequence . map (parseMaybe ruleP)

ruleP :: Parser (Bag, [(Int, Bag)])
ruleP = do
  bag <- bagP
  () <$ string " contain "
  values <-
    ([] <$ string "no other bags")
    <|>
    sepBy1 subbagP (string ", ")
  () <$ char '.'
  pure (bag, values)
  where
    subbagP :: Parser (Int, Bag)
    subbagP = (,) <$> (decimal <* space1) <*> bagP

    bagP :: Parser Bag
    bagP = do
      d1 <- some letterChar
      () <$ space1
      d2 <- some letterChar
      () <$ space1
      () <$ string "bag"
      () <$ optional (char 's')
      pure (d1, d2)
