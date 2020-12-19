import Control.Monad.State
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.IntMap.Strict as Map
import Data.IntMap (IntMap)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Ch = A | B deriving Show

type RawRule = Either Ch (NonEmpty (NonEmpty Int))

data Rule
  = Ch !Ch
  | Or !(NonEmpty (NonEmpty Rule))
  deriving Show

type Message = [Ch]

main :: IO ()
main = print . decode =<< readFile "Day19.input"

decode :: String -> Maybe (Rule, [Message])
decode s = do
  (rs, ms) <- parseMaybe inputP s
  let rm = Map.fromList rs
  guard $ Map.size rm == length rs
  r <- denormalize rm
  pure (r, ms)

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

denormalize :: IntMap RawRule -> Maybe Rule
denormalize rrs = go 0 `evalStateT` Map.empty
  where
    go k = do
      cache <- get
      case Map.lookup k cache of
        Just v  -> pure v
        Nothing -> do
          v <- lift $ Map.lookup k rrs
          case v of
            Left c   -> pure $ Ch c
            Right vs -> do
              r <- Or <$> traverse (traverse go) vs
              modify $ Map.insert k r
              pure r

example :: String
example = unlines
  [ "0: 4 1 5"
  , "1: 2 3 | 3 2"
  , "2: 4 4 | 5 5"
  , "3: 4 5 | 5 4"
  , "4: \"a\""
  , "5: \"b\""
  , ""
  , "ababbb"
  , "bababa"
  , "abbbab"
  , "aaabbb"
  , "aaaabbb"
  ]
