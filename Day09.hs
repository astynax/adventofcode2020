import Text.Read (readMaybe)

main :: IO ()
main = do
  print $ Just (127, 62) == solve 5
    [ 35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102
    , 117, 150, 182, 127, 219, 299, 277, 309, 576 ]
  print . (solve 25 =<<) . traverse readMaybe . lines
    =<< readFile "Day09.input"

solve :: Int -> [Int] -> Maybe (Int, Int)
solve p xs = do
  ((firstInvalid, _) : _) <-
     pure . dropWhile isValid $ withPreambles p xs

  (subseq : _) <-
     pure . filter ((== firstInvalid) . sum) $ subseqs xs

  pure (firstInvalid, minimum subseq + maximum subseq)
  where
    isValid (n, ps) = not $ null
      [ ()
      | x <- ps
      , y <- ps
      , x /= y
      , x + y == n
      ]

subseqs :: [a] -> [[a]]
subseqs xs =
  [ take t . drop d $ xs
  | d <- [0 .. l - 1]
  , t <- [2 .. l - d]
  ]
  where l = length xs

withPreambles :: Int -> [Int] -> [(Int, [Int])]
withPreambles s = drop s . go []
  where
    go _ []        = []
    go ps (x : xs) = (x, ps) : go (take s $ x : ps) xs
