main :: IO ()
main = print $ solve (8987316, 14681524)

solve :: (Int, Int) -> (Int, Int)
solve (pub1, pub2) =
  ( findStep pub1 `stepsFrom` pub2
  , 0
  )

step :: Int -> Int -> Int
step s x = x * s `mod` 20201227

stepsFrom :: Int -> Int -> Int
stepsFrom n s = head . drop n $ iterate (step s) 1

findStep :: Int -> Int
findStep n =
  fst . head . dropWhile ((/= n) . snd) . zip [0..] $ iterate (step 7) 1
