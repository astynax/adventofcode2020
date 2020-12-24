import Data.IORef
import Control.Monad.Reader
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector.Primitive.Mutable as Vector
import Data.Vector.Primitive.Mutable (IOVector, MVector(..))

type Input = [Int]

data Cups = Cups !Int !(NonEmpty Int) deriving (Eq, Show)
data Picked = Picked !(Int, Int, Int) !Cups deriving (Show)

main :: IO ()
main = do
  print selfTest
  -- pure functional solution
  print $ digitsAfter1 . unCups =<< 100 `times` move =<< start
    [1, 5, 7, 6, 2, 3, 9, 8, 4]
  --
  -- dlist-based solution
  -- task 1
  runCupsM [1, 5, 7, 6, 2, 3, 9, 8, 4] $ do
    replicateM_ 100 moveM
    digits <- listFromM =<< posM
    liftIO . print . digitsAfter1 $ digits
  -- task 2
  runCupsM ([1, 5, 7, 6, 2, 3, 9, 8, 4] <> [10 .. 1000000]) $ do
    replicateM_ 10000000 (moveM >> liftIO (putChar '.'))
    Just p <- lookupM 1
    (n1, p1) <- lookM ceRRef p
    (n2, _)  <- lookM ceRRef p1
    liftIO . print $ n1 * n2

unCups :: Cups -> [Int]
unCups (Cups c (x :| xs)) = c : x : xs

digitsAfter1 :: [Int] -> Maybe String
digitsAfter1 xs = do
  (as, (_:bs)) <- pure $ break (== 1) xs
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

newCurrent :: Cups -> Maybe Cups
newCurrent (Cups c (x :| xs)) =
  Cups x <$> NE.nonEmpty (xs <> [c])

pick :: Cups -> Maybe Picked
pick (Cups c (x :| xs)) = do
  (y : z : r : rs) <- pure xs
  pure $ Picked (x, y, z) $ Cups c $ r :| rs

getDest :: Picked -> Maybe Int
getDest (Picked (c1, c2, c3) (Cups c (x :| xs))) =
  calcDest c [c1, c2, c3] (maximum $ c : c1 : c2 : c3 : x : xs)

calcDest :: Int -> [Int] -> Int -> Maybe Int
calcDest f vs m = go (f - 1)
  where
    go i
      | i `elem` vs = go (i - 1)
      | i == f      = Nothing
      | i <  1      = go m
      | otherwise   = Just i

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

-- dlist on vectors

data CupsEnv = CupsEnv
  { cePos  :: !(IORef Int)
  , ceVals :: !(IOVector Int)
  , ceLRef :: !(IOVector Int)
  , ceRRef :: !(IOVector Int)
  }

newtype Pos = Pos Int deriving (Eq, Show)

type Pick = (Pos, [Int])

type CupsM = ReaderT CupsEnv IO

runCupsM :: Input -> CupsM a -> IO a
runCupsM is action = do
  iv <- newV is
  lv <- newV $ (l - 1) : [0 .. l - 2] -- n, 0, 1, ..
  rv <- newV $ [1 .. l - 1] <> [0]    -- 1, 2, 3, .. n, 0
  ref <- newIORef 0
  action `runReaderT` CupsEnv ref iv lv rv
  where
    l = length is
    newV xs = do
      v <- Vector.new l
      v <$ traverse (uncurry $ Vector.write v) (zip [0 ..] xs)

newCurrentM :: CupsM ()
newCurrentM = jumpM ceRRef

pickM :: CupsM Pick
pickM = do
  from <- posM
  (c1, p1) <- lookM ceRRef from
  (c2, p2) <- lookM ceRRef p1
  (c3, p3) <- lookM ceRRef p2
  (_,  to) <- lookM ceRRef p3
  linkM ceRRef from to
  linkM ceLRef to from
  pure (p1, [c1, c2, c3])

getDestM :: Pick -> CupsM Pos
getDestM (_, ps) = do
  v <- getM =<< posM
  s <- sizeM
  Just !d <- pure $ calcDest v ps s
  lookupM d >>= \case
    Just !p -> pure p
    Nothing -> error $ "Can't find a cup labeled with " <> show d

putAfterM :: Pos -> Pick -> CupsM ()
putAfterM after (p1, _) = do
  (_, before) <- lookM ceRRef after
  (_, p2) <- lookM ceRRef p1
  (_, p3) <- lookM ceRRef p2
  linkM ceRRef after p1
  linkM ceLRef p1 after
  linkM ceRRef p3 before
  linkM ceLRef before p3

moveM :: CupsM ()
moveM = do
  p <- pickM
  d <- getDestM p
  putAfterM d p
  newCurrentM

lookupM :: Int -> CupsM (Maybe Pos)
lookupM val
  | val >= 10 = pure . Just . Pos $ val - 1
  | otherwise = do
    vs@(MVector _ l _) <- asks ceVals
    go vs l 0
  where
    go _ l i | i >= l = pure Nothing
    go v l i          = do
      x <- liftIO $ Vector.read v i
      if x == val
        then pure (Just $ Pos i)
        else go v l (i + 1)

jumpM :: (CupsEnv -> IOVector Int) -> CupsM ()
jumpM getter = do
  pr <- asks cePos
  pv <- asks getter
  liftIO $ do
    p <- readIORef pr
    n <- Vector.read pv p
    writeIORef pr n

getM :: Pos -> CupsM Int
getM (Pos p) = asks ceVals >>= liftIO . (Vector.read `flip` p)

posM :: CupsM Pos
posM = asks cePos >>= fmap Pos . liftIO . readIORef

sizeM :: CupsM Int
sizeM = asks ceVals >>= \(MVector _ s _) -> pure s

lookM :: (CupsEnv -> IOVector Int) -> Pos -> CupsM (Int, Pos)
lookM getter (Pos from) = do
  vv <- asks ceVals
  pv <- asks getter
  pos <- liftIO $ Vector.read pv from
  val <- liftIO $ Vector.read vv pos
  pure (val, Pos pos)

linkM :: (CupsEnv -> IOVector Int) -> Pos -> Pos -> CupsM ()
linkM getter (Pos from) (Pos to) = do
  rv <- asks getter
  Vector.write rv from to

display :: CupsM ()
display = do
  pos <- posM
  liftIO $ print pos
  printV =<< asks ceVals
  printV =<< asks ceLRef
  printV =<< asks ceRRef
  liftIO $ putStrLn "---"
  liftIO . print =<< listFromM pos
  where
    printV v@(MVector _ l _) =
      liftIO $ print =<< traverse (Vector.read v) [0 .. l - 1]

listFromM :: Pos -> CupsM [Int]
listFromM p = go p
  where
    go i = do
      x <- getM i
      (_, n) <- lookM ceRRef i
      if n == p
        then pure [x]
        else (x :) <$> go n
