import Control.Monad (guard)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

type Range = (Int, Int)
type Field = String
type Ticket = [Int]
type Rule = (Range, Range)
type Rules = Map Field Rule
type Input = (Rules, Ticket, [Ticket])

main :: IO ()
main = print . (solve =<<) . decode =<< readFile "Day16.input"

solve :: Input -> Maybe (Int, Int)
solve (rules, ticket, tickets) = do
  let answer1 = sum $ concatMap (filter $ not . isValid) tickets
  let validTickets = filter (all isValid) tickets
  let columns = toColumns validTickets
  possibilities <- annotate columns rules
  assocs <- assocColumns possibilities
  fullTicket <- sequence
    [ (,x) <$> IntMap.lookup i assocs
    | (i, x) <- zip [0..] ticket
    ]
  answer2 <- do
    vs <- pure
      [ v
      | (f, v) <- fullTicket
      , take (length "departure") f == "departure"
      ]
    product vs <$ guard (length vs == 6)
  pure (answer1, answer2)
  where
    ranges = map snd $ Map.toList rules
    isValid field = any (`fit` field) ranges

fit :: Rule -> Int -> Bool
fit (r1, r2) x = x `inRange` r1 || x `inRange` r2
  where
    inRange v (a, b) = v >= a && v <= b

annotate :: [[Int]] -> Rules -> Maybe (IntMap (NonEmpty Field))
annotate cs rules = IntMap.fromList . zip [0..] <$> traverse findVariants cs
  where
    rs = Map.toList rules
    findVariants vs = NE.nonEmpty
      [ c
      | (c, rule) <- rs
      , all (fit rule) vs
      ]

assocColumns :: IntMap (NonEmpty Field) -> Maybe (IntMap Field)
assocColumns = go IntMap.empty . IntMap.toList
  where
    go acc [] = Just acc
    go acc ps =
      case catMaybes $ map fork variants of
        []    -> Nothing
        (v:_) -> Just v
      where
        fork (i, c) =
          go (IntMap.insert i c acc) =<< sequence
            [ (ii,) <$> (NE.nonEmpty $ filter (/= c) $ x : xs)
            | (ii, (x :| xs)) <- ps
            , ii /= i
            ]
        variants =
          [ (i, c)
          | (i, (x :| xs)) <- sortOn (NE.length . snd) ps
          , c <- x : xs
          ]

toColumns :: [Ticket] -> [[Int]]
toColumns = go []
  where
    go acc xs = case traverse uncons xs of
      Just ps -> let (a, xs') = unzip ps in go (a : acc) xs'
      Nothing -> reverse acc
    uncons = \case
      (x:xs) -> Just (x, xs)
      _      -> Nothing

decode :: String -> Maybe Input
decode = parseMaybe inputP
  where
    inputP :: Parser Input
    inputP = do
      rs <- some (ruleP <* newline)
      () <$ newline
      let
        rules = Map.fromList rs
        numberOfRules = Map.size rules
      guard (length rs == numberOfRules) <?> "All rules are unique"
      () <$ string "your ticket:" <* newline
      ticket <- ticketP <* newline
      () <$ newline <* string "nearby tickets:" <* newline
      tickets <- some (ticketP <* newline)
      guard (all ((== numberOfRules) . length) $ ticket : tickets)
        <?> "All tickets have " ++ show numberOfRules ++ " columns"
      () <$ eof
      pure (rules, ticket, tickets)

    ruleP :: Parser (Field, (Range, Range))
    ruleP = do
      field <- some (letterChar <|> char ' ')
      () <$ string ": "
      range1 <- rangeP
      () <$ string " or "
      range2 <- rangeP
      pure (field, (range1, range2))

    rangeP :: Parser Range
    rangeP = (,) <$> decimal <*> (char '-' *> decimal)

    ticketP :: Parser Ticket
    ticketP = decimal `sepBy1` char ','
