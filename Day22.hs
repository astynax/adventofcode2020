import Control.Monad.State.Strict
import Data.List (nub)
import qualified Data.HashSet as Set
import Debug.Trace (trace)
import Text.Read (readMaybe)

main :: IO ()
main = do
  print $ playRec [9, 2, 6, 3, 1] [5, 8, 4, 7, 10]
    == [7, 5, 6, 2, 4, 1, 10, 8, 9, 3]
  print . fmap solve . decode =<< readFile "Day22.input"

solve :: ([Int], [Int]) -> (Int, Int)
solve (p1, p2) =
  ( score $ play    p1 p2
  , score $ playRec p1 p2
  )

score :: [Int] -> Int
score = sum . zipWith (*) [1..] . reverse

play :: [Int] -> [Int] -> [Int]
play ls     []     = ls
play []     rs     = rs
play (l:ls) (r:rs)
  | l > r          = play (ls ++ [l, r]) rs
  | otherwise      = play ls             (rs ++ [r, l])

playRec :: [Int] -> [Int] -> [Int]
playRec d1 d2 = either id id $ go Set.empty d1 d2
  where
    go !v !p1 !p2 =
      case Set.member (p1, p2) v of
        True -> Left p1
        _    ->
          case (p1, p2) of
            (ls,     [])     -> Left  ls
            ([],     rs)     -> Right rs
            ((l:ls), (r:rs)) ->
              let
                v'      = Set.insert (p1, p2) v
                goLeft  = go v' (ls ++ [l, r]) rs
                goRight = go v' ls             (rs ++ [r, l])
              in if l > length ls || r > length rs
                 then
                   if l > r
                   then goLeft
                   else goRight
                 else
                   case go v' (take l ls) (take r rs) of
                     Left  _ -> goLeft
                     Right _ -> goRight

decode :: String -> Maybe ([Int], [Int])
decode xs = do
  (("Player 1:" : as), (_ : "Player 2:" : bs)) <- pure . break null $ lines xs
  d1 <- traverse readMaybe as
  d2 <- traverse readMaybe bs
  guard $ length (nub $ d1 <> d2) == length (d1 <> d2)
  pure (d1, d2)
