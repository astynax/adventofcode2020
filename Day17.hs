{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)

type (:*:) a b = (b, a)
infixl 5 :*:

class Splittable t where
  split :: t -> [t]

instance Splittable () where
  split () = [()]

instance Splittable a => Splittable (a :*: Int) where
  split = up split
    where
      up f (b, a)= (,) <$> near b <*> f a
      near x = [x - 1, x, x + 1]

class (Ord t, Ord s) => Expandable t s | t -> s where
  expand :: Set s -> Set t

instance {-# OVERLAPPABLE #-}
  (Ord t, Ord r, Expandable s r, Expandable t s) => Expandable t r where
  expand = expand . expand

instance {-# OVERLAPPING #-}  Expandable (() :*: Int) () where
  expand = Set.map (0,)

instance {-# OVERLAPPABLE #-}  Ord a => Expandable (a :*: Int) a where
  expand = Set.map (0,)

main :: IO ()
main = do
  display3d . expand . step $ decode ".#.\n..#\n###"
  print . solve . decode =<< readFile "Day17.input"

solve :: Set (() :*: Int :*: Int) -> (Int, Int)
solve w =
  ( Set.size $ 6 `times` step @(() :*: Int :*: Int :*: Int)         $ expand w
  , Set.size $ 6 `times` step @(() :*: Int :*: Int :*: Int :*: Int) $ expand w
  )

times :: Int -> (a -> a) -> a -> a
times n f = head . drop n . iterate f

step :: (Splittable a, Ord a) => Set a -> Set a
step !w = w `Set.difference` dead `Set.union` newborns
  where
    dead = Set.fromList
      [ n | (n, v) <- ns, v > 3 || v < 2 ]
    newborns = Set.fromList
      [ n | (n, v) <- ns, v == 3 ]
    ns = neibs split w

neibs :: Ord a => (a -> [a]) -> Set a -> [(a, Int)]
neibs near w = Map.toList
  $ foldl' count (Map.fromSet (const 0) w)
  [ n
  | p <- Set.toList w
  , n <- near p
  , n /= p
  ]
  where
    count = flip (Map.insertWith (+) `flip` (1 :: Int))

decode :: String -> Set (() :*: Int :*: Int)
decode s = Set.fromList
  [ (y, (x, ()))
  | (y, row) <- zip [0..] $ lines s
  , (x, c)   <- zip [0..] row
  , c == '#'
  ]

display3d :: Set (() :*: Int :*: Int :*: Int) -> IO ()
display3d w = mapM_ layer
  [ ( z
    , [ [ if Set.member (z, (y, (x, ()))) w then '#' else '.'
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
    (nx, mx) = dims $ \(_, (_, (x, ()))) -> x
    (ny, my) = dims $ \(_, (y, _      )) -> y
    (nz, mz) = dims $ \(z, _           ) -> z
