import Data.List (sort, group)
import Text.Read (readMaybe)

data Diff = D1 | D3 deriving (Eq, Show)

main :: IO ()
main = do
  print $ Just 19208 == (
    countRendundant <$> diffs
      [ 28, 33, 18, 42, 31, 14, 46, 20, 48, 47
      , 24, 23, 49, 45, 19, 38, 39, 11, 1, 32
      , 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3
      ])
  print . (solve =<<) . sequence . map readMaybe . lines
    =<< readFile "Day10.input"

solve :: [Int] -> Maybe (Int, Int)
solve xs = do
  ds <- diffs xs
  let distribution = count D1 ds * (1 + count D3 ds) -- 1 for internal adapter
  let variants = countRendundant ds
  pure (distribution, variants)
  where
    count x = length . filter (== x)

diffs :: [Int] -> Maybe [Diff]
diffs = sequence . map toDiff . deltas . sort
  where
    deltas xs = zipWith (-) xs (0 : xs)
    toDiff 1 = Just D1
    toDiff 3 = Just D3
    toDiff _ = Nothing

countRendundant :: [Diff] -> Int
countRendundant = product . map (countVariations . length) . filter ones . group
  where
    ones (D1 : _) = True
    ones _        = False
    countVariations = (allVariations !!)
    allVariations = map variations [0..]

variations :: Int -> Int
variations = length . variate . (`replicate` 1)
  where
    variate (a : b : xs)
      | a < 3     = keepA ++ variate ((a + b) : xs)
      | otherwise = keepA
      where
        keepA = map (a :) $ variate $ b : xs
    variate xs    = [xs]
