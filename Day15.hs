import qualified Data.IntMap as Map
import Data.IntMap (IntMap)

data State = State
  { spoken  :: !(IntMap Int)
  , lastOne :: !Int
  , turn    :: !Int
  } deriving (Show)

main :: IO ()
main = print $ solve [0, 5, 4, 1, 10, 14, 7]

solve :: [Int] -> Maybe (Int, Int)
solve xs = do
  s <- initState xs
  pure
    ( lastOne $ runUntil 2020 s
    , lastOne $ runUntil 30000000 s
    )

runUntil :: Int -> State -> State
runUntil n !s
  | turn s == n = s
  | otherwise   = runUntil n $ step s

step :: State -> State
step !s@(State{..}) = case Map.lookup lastOne spoken of
  Just t
    | t == turn  -> next{ lastOne = 0 }
    | otherwise  -> next
      { lastOne = turn - t
      , spoken  = Map.insert lastOne turn spoken
      }
  _              -> next
    { lastOne = 0
    , spoken  = Map.insert lastOne turn spoken
    }
  where
    next = s{ turn = turn + 1 }

initState :: [Int] -> Maybe State
initState [] = Nothing
initState xs = Just State{..}
  where
    spoken = Map.fromList $ zip xs [1..]
    lastOne = last xs
    turn = length xs
