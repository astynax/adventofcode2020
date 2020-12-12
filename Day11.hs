{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

type Pos = (Int, Int)
type Cells = Set Pos
type Neibs = [Pos]

data Future
data Near
data Visible

data Setup t = Setup
  { mx    :: Int
  , my    :: Int
  , seats :: Map Pos Neibs
  }

class Steppable t where
  tolerate :: Int -> Bool
  prepareSeat :: Setup Future -> Pos -> Neibs -> Neibs

instance Steppable Near where
  tolerate = (< 4)
  prepareSeat Setup{..} (x, y) =
    filter (`Map.member` seats) . map (\(dx, dy) -> (x + dx, y + dy))

instance Steppable Visible where
  tolerate = (< 5)
  prepareSeat Setup{..} seat ds =
    [ neib
    | dir <- ds
    , Just neib <- [lookFrom seat dir]
    ]
    where
      lookFrom (cx, cy) (dx, dy)
        | x < 0 || x > mx || y < 0 || y > my = Nothing
        | Map.member (x, y) seats            = Just (x, y)
        | otherwise                          = lookFrom (x, y) (dx, dy)
        where
          (x, y) = (cx + dx, cy + dy)

main :: IO ()
main = do
  selfCheck
  print . solve . decode . lines =<< readFile "Day11.input"

solve :: Setup Future -> (Int, Int)
solve s =
  ( stabilize @Near s
  , stabilize @Visible s
  )

stabilize :: forall t. Steppable t => Setup Future -> Int
stabilize s = go Set.empty
  where
    setup = prepare @t s
    go !old
      | old == new = Set.size old
      | otherwise  = go new
      where
        new = step @t setup old

step :: forall t. Steppable t => Setup t -> Cells -> Cells
step Setup{..} !os = Set.union emptySeats nonLeaved
  where
    allSeats = Map.keysSet seats
    emptySeats = Set.difference (Set.difference allSeats os)
      $ Map.keysSet neibMap
    nonLeaved = Set.difference os
      $ Set.fromList . map fst . filter (not . tolerate @t . snd)
      $ Map.toList neibMap
    neibMap = foldl' (flip $ uncurry $ Map.insertWith (+)) Map.empty
      [ (neib, 1)
      | pos <- Set.toList os
      , Just neibs <- [Map.lookup pos seats]
      , neib <- neibs
      ]

prepare :: forall t. Steppable t => Setup Future -> Setup t
prepare s@(Setup{..}) = Setup mx my $ Map.mapWithKey (prepareSeat @t s) seats

decode :: [String] -> Setup Future
decode rows = Setup {..}
  where
    my = length rows - 1
    mx = case rows of
      (r : _) -> length r - 1
      _       -> 0
    seats = Map.fromList
      [ ((x, y), diffs)
      | (y, row)  <- zip [0..] rows
      , (x, cell) <- zip [0..] row
      , cell == 'L'
      ]
    diffs =
      [ (dx, dy)
      | dx <- [-1, 0, 1]
      , dy <- [-1, 0, 1]
      , dx /= 0 || dy /= 0
      ]

---

display :: Setup t -> Cells -> [String]
display Setup{..} cs = map showRow [0 .. my]
  where
    showRow y =
      [ if Set.member (x, y) cs then '#' else
          if Map.member (x, y) seats then 'L' else '.'
      | x <- [0 .. mx]
      ]

selfCheck :: IO ()
selfCheck = do
  print $ display setup (s $ s $ s Set.empty) ==
    [ "#.##.L#.##"
    , "#L###LL.L#"
    , "L.#.#..#.."
    , "#L##.##.L#"
    , "#.##.LL.LL"
    , "#.###L#.##"
    , "..#.#....."
    , "#L######L#"
    , "#.LL###L.L"
    , "#.#L###.##"
    ]
  print $ (37, 26) == solve (decode example)
  where
    s = step setup
    setup = prepare @Near $ decode example

example :: [String]
example =
  [ "L.LL.LL.LL"
  , "LLLLLLL.LL"
  , "L.L.L..L.."
  , "LLLL.LL.LL"
  , "L.LL.LL.LL"
  , "L.LLLLL.LL"
  , "..L.L....."
  , "LLLLLLLLLL"
  , "L.LLLLLL.L"
  , "L.LLLLL.LL"
  ]
