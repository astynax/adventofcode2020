main :: IO ()
main = print . solve . map read . lines =<< readFile "Day01.input"

solve :: [Int] -> (Int, Int)
solve rows = (head pairs, head triples)
  where
    erows = zip [0 :: Int ..] rows
    pairs =
      [ x * y
      | (ix, x) <- erows
      , (iy, y) <- erows
      , ix /= iy
      , x + y == 2020
      ]
    triples =
      [ x * y * z
      | (ix, x) <- erows
      , (iy, y) <- erows
      , (iz, z) <- erows
      , ix /= iy
      , ix /= iz
      , iy /= iz
      , x + y + z == 2020
      ]
