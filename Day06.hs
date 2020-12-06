import Control.Monad (guard)
import Data.List (unfoldr, foldl')
import qualified Data.Set as Set

main :: IO ()
main = print . solve . decode . lines =<< readFile "Day06.input"

decode :: [String] -> [[String]]
decode = unfoldr $ \l -> do
  let (x, xs) = break null l
  guard . not . null $ x
  pure (x, drop 1 xs)

solve :: [[String]] -> (Int, Int)
solve groups = (countOnly atLeastOnce, countOnly common)
  where
    groupSets     = map (map Set.fromList) groups
    countOnly f   = sum $ map (length . f) groupSets
    atLeastOnce   = foldl' Set.union Set.empty
    common [x]    = x
    common (x:xs) = foldl' Set.intersection x xs
