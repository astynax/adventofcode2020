{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Identity
import Control.Monad.State
import qualified Data.IntMap.Strict as Map
import Data.IntMap (IntMap)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Void (Void)
import Text.Megaparsec hiding (match)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Ch = A | B deriving (Show, Eq)
type Message = [Ch]

type RawRule = Either Ch (NonEmpty (NonEmpty Int))
type RawRules = IntMap RawRule

newtype Simple f = Simple (NonEmpty (NonEmpty f))

data Loop f
  = LOr (NonEmpty (NonEmpty f))
  | Loop (NonEmpty f) [f]

data Rule (f :: Type -> Type)
  = Ch !Ch
  | Or !(f (Rule f))

deriving instance Show (f (Rule f)) => Show (Rule f)

class Branching t where
  split :: t a -> NonEmpty (NonEmpty a)
  wrap :: NonEmpty (NonEmpty a) -> t a

instance Branching Simple where
  split (Simple x) = x
  wrap = Simple

instance Branching Loop where
  split (LOr x)      = x
  split (Loop xs ys) =
    case NE.take 5 variants of  -- workaround :(
      (v: vs) -> v :| vs
      _       -> error "Impossible!"
    where
      variants = (xs <+ ys) <| fmap (\v -> xs <> v <+ ys) (split (Loop xs ys))
      (a :| as) <+ bs = a :| (as ++ bs)
  wrap = LOr

main :: IO ()
main = print . (solve =<<) . decode =<< readFile "Day19.input"

solve :: (RawRules, [Message]) -> Maybe (Int, Int)
solve (rm, ms) = do
  r1 <- denormalize @Simple (const id) $ rm
  r2 <- denormalize patch              $ rm
  pure
    ( length $ filter (satisfies r1) ms
    , length $ filter (satisfies r2) ms
    )

patch :: Int -> Rule Loop -> Rule Loop
patch k r = case (k, r) of
  (8,  Or (LOr ((r1 :| [])   :| []))) -> Or $ Loop (r1 :| []) []
  (11, Or (LOr ((r1 :| [r2]) :| []))) -> Or $ Loop (r1 :| []) [r2]
  _                                   -> r

satisfies :: Branching a => Rule a -> Message -> Bool
satisfies r m = not . null . filter (== (m, [])) $ match r m

match :: Branching a => Rule a -> Message -> [(Message, Message)]
match _       []     = []
match (Ch A)  (A:xs) = [([A], xs)]
match (Ch B)  (B:xs) = [([B], xs)]
match (Ch _)  _      = []
match (Or rs) s      = concat . NE.toList . fmap (testAll `flip` s) $ split rs

testAll :: Branching a => NonEmpty (Rule a) -> Message -> [(Message, Message)]
testAll _         [] = []
testAll (a :| as) s  = do
  (m1, s')  <- match a s
  case as of
    []     -> pure (m1, s')
    (x:xs) -> do
      (ms, s'') <- testAll (x :| xs) s'
      pure (m1 ++ ms, s'')

decode :: String -> Maybe (IntMap RawRule, [Message])
decode s = do
  (rs, ms) <- parseMaybe inputP s
  let rm = Map.fromList rs
  guard $ Map.size rm == length rs
  pure (rm, ms)

inputP :: Parser ([(Int, RawRule)], [Message])
inputP = do
  rs <- many (rawRuleP <* newline)
  () <$ newline
  ms <- many (messageP <* newline)
  pure (rs, ms)
  where
    messageP = some $ (A <$ char 'a') <|> (B <$ char 'b')
    rawRuleP = do
      i <- decimal
      () <$ string ": "
      r <- (Left <$> chP) <|> (Right <$> orP)
      pure (i, r)
      where
        chP = (A <$ string "\"a\"") <|> (B <$ string "\"b\"")
        orP = (decimal `neSepBy` char ' ') `neSepBy` string " | "

neSepBy :: Parser a -> Parser b -> Parser (NonEmpty a)
neSepBy pv ps = do
  v <- pv
  (NE.cons v <$> try (ps *> neSepBy pv ps))
    <|> pure (v :| [])

denormalize
  :: Branching a => (Int -> Rule a -> Rule a)
  -> IntMap RawRule
  -> Maybe (Rule a)
denormalize f rrs = go 0 `evalStateT` Map.empty
  where
    go k = do
      cache <- get
      case Map.lookup k cache of
        Just v  -> pure v
        Nothing -> do
          v <- lift $ Map.lookup k rrs
          case v of
            Left c   -> pure . f k $ Ch c
            Right vs -> do
              r <- f k . Or . wrap <$> traverse (traverse go) vs
              modify $ Map.insert k r
              pure r
