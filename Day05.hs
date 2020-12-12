main :: IO ()
main = do
  print $ (44, 5) == decode "FBFBBFFRLR"
  print . solve . map decode . lines =<< readFile "Day05.input"

decode :: String -> (Int, Int)
decode s =
  ( binSearch ('F' ==) $ take 7 s
  , binSearch ('L' ==) $ drop 7 s
  )
  where
    binSearch p vs = go (truncate ((2 :: Double) ^^ length vs) - 1) vs
      where
        go v []     = v
        go v (x:xs) =
          let hv  = v `div` 2
              res = go hv xs
          in res + if p x then 0 else (hv + 1)

solve :: [(Int, Int)] -> (Int, Int)
solve passes = (maximum ids, seat)
  where
    ids = map toId passes
    toId (r, c) = 8 * r + c
    seat = case seats of
      []      -> -1
      (x : _) -> x
    seats =
      [ x
      | x <- [toId (1, 0) .. toId (126, 7)]
      , not (x `elem` ids)
      , (x - 1) `elem` ids
      , (x + 1) `elem` ids
      ]
