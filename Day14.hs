{-# LANGUAGE DerivingStrategies #-}

import Control.Monad (guard)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Read (readMaybe)

type Parser = Parsec Void String

newtype U36 = U36 IntSet deriving stock (Show, Eq)

data Mask = Mask
  { os :: !U36
  , zs :: !U36
  } deriving (Show)

data Put = Put
  { adr :: !Int
  , val :: !U36
  } deriving (Show)

data State = State
  { mem  :: !(IntMap U36)
  , mask :: !Mask
  }

type Cmd = Either Mask Put
type Putter = Mask -> Int -> U36 -> IntMap U36

main :: IO ()
main = do
  -- self-check
  print $ do
    [Left m, Right (Put 0 v)] <- decode
      [ "mask = 1XXXXXXXXXXXXXXXXXXXXXXXXX011XXXX00X"
      , "mem[0] = 255" ]
    pure $ v `withMask` m == U36 (Set.fromList [0, 3, 4, 5, 6, 7, 8, 9, 35])
  print . fmap solve . decode . lines =<< readFile "Day14.input"

solve :: [Cmd] -> (Integer, Integer)
solve cs =
  ( sumMem . runWith maskVal  cs $ State Map.empty initMask
  , sumMem . runWith maskAddr cs $ State Map.empty initMask
  )
  where
    sumMem = sum . map (fromIntegral . fromBits . snd) . Map.toList . mem

initMask :: Mask
initMask = Mask (U36 Set.empty) (U36 Set.empty)

runWith :: Putter -> [Cmd] -> State -> State
runWith _ []        s             = s
runWith f (c : cs) !s@(State{..}) = runWith f cs $ case c of
  Left m          -> s{ mask = m }
  Right (Put a v) -> s{ mem = Map.union (f mask a v) mem }

maskVal :: Putter
maskVal m a v = Map.singleton a (v `withMask` m)

maskAddr :: Putter
maskAddr Mask{..} a v = Map.fromList
  [ (fromBits $ fixPart .|. varPart, v)
  | varPart <- map (U36 . Set.fromList) (variants xs)
  ]
  where
    xs = inv (zs .|. os)
    fixPart = toBits a .&. zs .|. os
    variants (U36 s) = go $ Set.toList s
      where
        go []     = [[]]
        go (b:bs) =
          let bs' = go bs
          in map (b :) bs' ++ bs'

withMask :: U36 -> Mask -> U36
withMask x Mask{..} = x .|. os .-. zs

-- decoding

decode :: [String] -> Maybe [Cmd]
decode = traverse (parseMaybe cmdP)

cmdP :: Parser (Either Mask Put)
cmdP = (Left <$> maskP) <|> (Right <$> putP)
  where
    maskP = do
      () <$ string "mask = "
      bits <- some $ (Just <$> bitP) <|> (Nothing <$ char 'X')
      guard $ length bits == 36
      pure $ Mask
        (decodeBits (Just True) bits)
        (decodeBits (Just False) bits)

    putP = do
      () <$ string "mem["
      Just adr <- readMaybe <$> some digitChar
      () <$ string "] = "
      Just val <- (fmap toBits . readMaybe) <$> some digitChar
      pure Put{..}

    bitP = (True <$ char '1') <|> (False <$ char '0')

decodeBits :: Eq a => a -> [a] -> U36
decodeBits one bs = U36 $ Set.fromList
  [ i
  | (i, x) <- zip [0..] $ reverse bs
  , x == one
  ]

-- working with U36

toBits :: Int -> U36
toBits = U36 . Set.fromList . go 0 . abs
  where
    go _ 0 = []
    go b x
      | r == 1    = b : bs
      | otherwise =     bs
      where
        (a, r) = divMod x 2
        bs = go (b + 1) a

fromBits :: U36 -> Int
fromBits (U36 x) = sum $ map (truncate @Double . (2 ^^)) $ Set.toList x

inv :: U36 -> U36
inv = (U36 (Set.fromList [0..35]) .-.)

(.&.), (.|.), (.-.) :: U36 -> U36 -> U36
(.&.) = liftU36 Set.intersection
(.|.) = liftU36 Set.union
(.-.) = liftU36 Set.difference

liftU36 :: (IntSet -> IntSet -> IntSet) -> U36 -> U36 -> U36
liftU36 f (U36 a) (U36 b) = U36 $ f a b
