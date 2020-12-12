import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List (foldl')
import Text.Read (readMaybe)

data Dir = N | S | E | W deriving (Show, Eq)
data Rot = L | R deriving (Show, Eq)
data Cmd
  = Dir Dir
  | Rot Rot
  | Fwd
  deriving (Show, Eq)

data Ship = Ship
  { wpx :: !Int
  , wpy :: !Int
  , x   :: !Int
  , y   :: !Int
  , dir :: !Dir
  } deriving Show

main :: IO ()
main = do
  print $ Just (25, 286) == (
    solve <$> traverse decode ["F10", "N3", "F7", "R90", "F11"])
  print . fmap solve . traverse decode . lines =<< readFile "Day12.input"

shipAtStart :: Ship
shipAtStart = Ship 0 0 0 0 E

solve :: [(Cmd, Int)] -> (Int, Int)
solve cs =
  ( dist $ follow step shipAtStart cs
  , dist $ follow stepWP shipAtStart{ wpx = -10, wpy = -1 } cs
  )
  where
    dist = (+) <$> abs . x <*> abs . y

follow :: (Cmd -> Int -> Ship -> Ship) -> Ship -> [(Cmd, Int)] -> Ship
follow = foldl' . flip . uncurry

step :: Cmd -> Int -> Ship -> Ship
step c n !ship = case c of
  Dir N -> ship{ y = y ship - n }
  Dir S -> ship{ y = y ship + n }
  Dir E -> ship{ x = x ship - n }
  Dir W -> ship{ x = x ship + n }
  Fwd   -> step (Dir $ dir ship) n ship
  Rot r | n > 0
        -> step (Rot r) (n - 1) ship{ dir = rotate r (dir ship) }
  _     -> ship

rotate :: Rot -> Dir -> Dir
rotate L N = W
rotate L W = S
rotate L S = E
rotate L E = N
rotate R N = E
rotate R E = S
rotate R S = W
rotate R W = N

stepWP :: Cmd -> Int -> Ship -> Ship
stepWP c n !ship = case c of
  Dir N -> ship{ wpy = wpy ship - n }
  Dir S -> ship{ wpy = wpy ship + n }
  Dir E -> ship{ wpx = wpx ship - n }
  Dir W -> ship{ wpx = wpx ship + n }
  Fwd   -> ship
    { x = x ship + wpx ship * n
    , y = y ship + wpy ship * n
    }
  Rot r | n > 0
        -> stepWP (Rot r) (n - 1) $ rotateWP r ship
  _     -> ship

rotateWP :: Rot -> Ship -> Ship
rotateWP L !ship@(Ship{..}) = ship{ wpx = -wpy, wpy = wpx}
rotateWP R !ship@(Ship{..}) = ship{ wpx = wpy, wpy = -wpx}

decode :: String -> Maybe (Cmd, Int)
decode s = do
  (c : n) <- pure s
  cmd <- readCmd c
  guard $ n == filter isDigit n
  rawNum <- readMaybe n
  num <-
    if cmd /= Rot L && cmd /= Rot R
    then pure rawNum
    else do
      (x, 0) <- pure $ divMod rawNum 90
      pure x
  pure (cmd, num)
  where
    readCmd = \case
      'F' -> Just Fwd
      'L' -> Just $ Rot L
      'R' -> Just $ Rot R
      'N' -> Just $ Dir N
      'S' -> Just $ Dir S
      'E' -> Just $ Dir E
      'W' -> Just $ Dir W
      _   -> Nothing
