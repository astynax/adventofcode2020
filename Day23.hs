import Control.Monad (guard)
import Debug.Trace (trace)
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

type Input = [Int]

data Cups = Cups !Int !(NonEmpty Int) deriving (Eq, Show)
data Picked = Picked !(Int, Int, Int) !Cups deriving (Show)

main :: IO ()
main = do
  print selfTest
  print $ digitsAfter1 =<< 100 `times` move =<< start
    [1, 5, 7, 6, 2, 3, 9, 8, 4]
  print $ digitsAfter1 =<< 10000000 `times` move =<< start2
    [1, 5, 7, 6, 2, 3, 9, 8, 4]

digitsAfter1 :: Cups -> Maybe String
digitsAfter1 (Cups c (x :| xs)) = do
  (as, (_:bs)) <- pure $ break (== 1) $ c : x : xs
  pure $ concatMap show $ bs <> as

times :: Int -> (a -> Maybe a) -> a -> Maybe a
times n f x
  | n <= 0    = Just x
  | otherwise = ((n - 1) `times` f) =<< f x

start :: Input -> Maybe Cups
start is = do
  guard $ nub is == is
  (c : x : xs) <- pure is
  pure $ Cups c $ x :| xs

start2 :: Input -> Maybe Cups
start2 is = do
  Cups c (x :| xs) <- start is
  pure $ Cups c $ x :| xs <> [(maximum is + 1) .. 1000000]

newCurrent :: Cups -> Maybe Cups
newCurrent (Cups c (x :| xs)) =
  Cups x <$> NE.nonEmpty (xs <> [c])

pick :: Cups -> Maybe Picked
pick (Cups c (x :| xs)) = do
  (y : z : r : rs) <- pure xs
  pure $ Picked (x, y, z) $ Cups c $ r :| rs

getDest :: Picked -> Maybe Int
getDest (Picked _ (Cups c (x :| xs))) = go (c - 1) (x:xs)
  where
    go _ [] = Nothing
    go v vs = case filter (== v) vs of
      [] | v == c    -> Nothing
         | v <= 1    -> go (maximum vs) vs
         | otherwise -> go (v - 1)      vs
      _              -> Just v

putAfter :: Int -> Picked -> Maybe Cups
putAfter d (Picked (c1, c2, c3) (Cups c (x :| xs))) = do
  (as, (b : bs)) <- pure $ break (== d) $ x : xs
  Cups c <$> NE.nonEmpty (as <> (b : c1 : c2 : c3 : bs))

move :: Cups -> Maybe Cups
move c = do
  p <- pick c
  d <- getDest p
  putAfter d p >>= newCurrent

selfTest :: Bool
selfTest = and
  [ getDest (Picked (4, 6, 7) $ Cups 5 $ 8 :| [9, 1, 3, 2]) == Just 3
  , getDest (Picked (8, 9, 1) $ Cups 2 $ 5 :| [4, 6, 7, 3]) == Just 7
  , putAfter 7 (Picked (8, 9, 1) $ Cups 2 $ 5 :| [4, 6, 7, 3])
    == Just (Cups 2 $ 5 :| [4, 6, 7, 8, 9, 1, 3])
  , (10 `times` move =<< start [3, 8, 9, 1, 2, 5, 4, 6, 7])
    == Just (Cups 8 $ 3 :| [7, 4, 1, 9, 2, 6, 5])
  ]
