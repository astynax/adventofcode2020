import Debug.Trace (trace)

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Dir = E | SE | SW | W | NW | NE deriving Show

-- 1 2 3    <->  1 2 3
-- 4 5 6 7  <-> 4 5 6 7
--   8 9 0  <->  8 9 0
type Floor = Set (Int, Int)

main :: IO ()
main = do
  print selfTest
  display $ step $ makeFloor example
  print . fmap solve . traverse decode . lines =<< readFile "Day24.input"

solve :: [[Dir]] -> (Int, Int)
solve xs =
  let
    newFloor = makeFloor xs
    answer1 = Set.size                      newFloor
    answer2 = Set.size $ 100 `times` step $ newFloor
  in (answer1, answer2)

times :: Int -> (a -> a) -> a -> a
times n f = head . drop n . iterate f

makeFloor :: [[Dir]] -> Floor
makeFloor =
  Map.keysSet . Map.filter id
  . foldl' (flip $ Map.insertWith xor `flip` True) Map.empty
  . map (`moves` (0, 0))

xor :: Bool -> Bool -> Bool
a `xor` b = (a && not b) || (b && not a)

step :: Floor -> Floor
step !old = old `Set.difference` dead `Set.union` new
  where
    dead = Set.fromList
      [ p
      | p <- Set.toList old
      , let v = fromMaybe 0 $ Map.lookup p ns
      , v < 1 || v > 2
      ]
    new  = Set.fromList [ p | (p, 2) <- Map.toList ns ]
    ns   = neibs old

neibs :: Floor -> Map (Int, Int) Int
neibs old =
  foldl' (flip $ Map.insertWith (+) `flip` (1 :: Int)) Map.empty
  [ move dir p
  | p <- Set.toList old
  , dir <- [E, SE, SW, W, NW, NE]
  ]

-- NE NW
-- E  ..  W
--    SE SW
move :: Dir -> (Int, Int) -> (Int, Int)
move d (x, y) = case d of
  E  -> (x - 1, y)
  SE -> (x,     y + 1)
  SW -> (x + 1, y + 1)
  W  -> (x + 1, y)
  NW -> (x,     y - 1)
  NE -> (x - 1, y - 1)

moves :: [Dir] -> (Int, Int) -> (Int, Int)
moves = flip $ foldl' (flip move)

decode :: String -> Maybe [Dir]
decode = parseMaybe (many dirP)

dirP :: Parser Dir
dirP =
  (    E  <$ char 'e')
  <|> (SE <$ string "se")
  <|> (SW <$ string "sw")
  <|> (W  <$ char 'w')
  <|> (NW <$ string "nw")
  <|> (NE <$ string "ne")

selfTest :: Bool
selfTest = and
  [ moves [NW, W, SW, E, E] (0, 0) == (0, 0)
  ]

example :: [[Dir]]
Just example = traverse decode
  [ "sesenwnenenewseeswwswswwnenewsewsw"
  , "neeenesenwnwwswnenewnwwsewnenwseswesw"
  , "seswneswswsenwwnwse"
  , "nwnwneseeswswnenewneswwnewseswneseene"
  , "swweswneswnenwsewnwneneseenw"
  , "eesenwseswswnenwswnwnwsewwnwsene"
  , "sewnenenenesenwsewnenwwwse"
  , "wenwwweseeeweswwwnwwe"
  , "wsweesenenewnwwnwsenewsenwwsesesenwne"
  , "neeswseenwwswnwswswnw"
  , "nenwswwsewswnenenewsenwsenwnesesenew"
  , "enewnwewneswsewnwswenweswnenwsenwsw"
  , "sweneswneswneneenwnewenewwneswswnese"
  , "swwesenesewenwneswnwwneseswwne"
  , "enesenwswwswneneswsenwnewswseenwsese"
  , "wnwnesenesenenwwnenwsewesewsesesew"
  , "nenewswnwewswnenesenwnesewesw"
  , "eneswnwswnwsenenwnwnwwseeswneewsenese"
  , "neswnwewnwnwseenwseesewsenwsweewe"
  , "wseweeenwnesenwwwswnew"
  ]

display :: Floor -> IO ()
display f = mapM_ putStrLn
  [ replicate n ' ' <> r
  | (r, n) <- zip rows [length rows, length rows - 1 .. 0]
  ]
  where
    rows =
      [ concat
        [ if (x, y) `Set.member` f then "# " else "  "
        | x <- [mix .. mx]
        ]
      | y <- [miy .. my]
      ]
    cs = Set.toList f
    mix = minimum $ map fst cs
    mx  = maximum $ map fst cs
    miy = minimum $ map snd cs
    my  = maximum $ map snd cs
