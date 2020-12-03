import Data.Bool (bool)

main :: IO ()
main = print . solve . lines =<< readFile "Day03.input"

solve :: [String] -> (Int, Int)
solve = ((,) <$> task1 <*> task2) . growRight
  where
    growRight = map (concat . repeat)
    task1     = checkSlope (3, 1)
    task2 l   = product $ map (`checkSlope` l)
      [ (1, 1)
      , (3, 1)
      , (5, 1)
      , (7, 1)
      , (1, 2)
      ]

checkSlope :: (Int, Int) -> [String] -> Int
checkSlope (dx, dy) rows = sum $ zipWith check [0, dx ..] $ rows'
  where
    rows' = [row | (i, row) <- zip [0..] rows, i `mod` dy == 0]
    check n = bool 0 1 . (== '#') . head . drop n
