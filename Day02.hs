import Data.Maybe (fromJust)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Rule = (Int, Int, Char)

main :: IO ()
main = print =<< solve . map parseLine . lines <$> readFile "Day02.input"

parseLine :: String -> (Rule, String)
parseLine = fromJust . parseMaybe ruleP
  where
    ruleP :: Parsec Void String (Rule, String)
    ruleP = do
      minO <- decimal
      char '-'
      maxO <- decimal
      space
      ch <- letterChar
      char ':'
      space
      pass <- some letterChar
      pure ((minO, maxO, ch), pass)


solve :: [(Rule, String)] -> (Int, Int)
solve = (,) <$> count isValidForTask1 <*> count isValidForTask2
  where
    count = (length .) . filter

    isValidForTask1 ((minO, maxO, c), cs) = n >= minO && n <= maxO
      where
        n = length $ filter (== c) cs

    isValidForTask2 ((pos1, pos2, c), cs) = length matches == 1
      where
        matches = [() | (i, x) <- zip [1..] cs, x == c, i == pos1 || i == pos2]
