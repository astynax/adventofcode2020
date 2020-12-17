{-# LANGUAGE TypeOperators #-}

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Set (Set)

type (:*:) a b = (a, b)
infixr 5 :*:

main :: IO ()
main = do
  -- display $ steps 3 $ decode ".#.\n..#\n###"
  -- print . solve . decode $ ".#.\n..#\n###"
  print . solve . decode =<< readFile "Day17.input"

solve :: Set (Int :*: Int) -> (Int, Int)
solve w =
  --Set.size $ 6 `times` step (up $         axis) $                 w  -- 2D
  ( Set.size $ 6 `times` step (up $ up      near) $          expand w  -- 3D
  , Set.size $ 6 `times` step (up $ up $ up near) $ expand $ expand w  -- 4D
  )

times :: Int -> (a -> a) -> a -> a
times n f = head . drop n . iterate f

step :: Ord a => (a -> [a]) -> Set a -> Set a
step scan !w = w `Set.difference` dead `Set.union` newborns
  where
    dead = Set.fromList
      [ n | (n, v) <- ns, v > 3 || v < 2 ]
    newborns = Set.fromList
      [ n | (n, v) <- ns, v == 3 ]
    ns = neibs scan w

neibs :: Ord a => (a -> [a]) -> Set a -> [(a, Int)]
neibs scan w = Map.toList
  $ foldl' count (Map.fromSet (const 0) w)
  [ n
  | p <- Set.toList w
  , n <- scan p
  , n /= p
  ]
  where
    count = flip (Map.insertWith (+) `flip` (1 :: Int))

near :: Int -> [Int]
near x = [x - 1, x, x + 1]

up :: (a -> [a]) -> a :*: Int -> [a :*: Int]
up f (a, b)= (,) <$> f a <*> near b

decode :: String -> Set (Int :*: Int)
decode s = Set.fromList
  [ (x, y)
  | (y, row) <- zip [0..] $ lines s
  , (x, c)   <- zip [0..] row
  , c == '#'
  ]

expand :: Ord a => Set a -> Set (a :*: Int)
expand = Set.map (, 0)

display3d :: Set (Int :*: Int :*: Int) -> IO ()
display3d w = mapM_ layer
  [ ( z
    , [ [ if Set.member (z, (x, y)) w then '#' else '.'
        | x <- [nx .. mx] ]
      | y <- [ny .. my]
      ]
    )
  | z <- [nz .. mz]
  ]
  where
    layer (z, rows) = do
      putStrLn $ "z=" ++ show z
      mapM_ putStrLn rows
      putStrLn ""
    cs = Set.toList w
    dims f = ((,) <$> minimum <*> maximum) $ map f cs
    (nx, mx) = dims $ \(_, (_, x)) -> x
    (ny, my) = dims $ \(_, (y, _)) -> y
    (nz, mz) = dims $ \(z, _     ) -> z
