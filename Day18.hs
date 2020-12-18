import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Op = Add | Mul deriving Show

data Expr
  = Num Int
  | Bin Op Expr Expr
  | Br Expr
  deriving Show

data MulE = MulE AddE [AddE] deriving Show

data AddE
  = ANum Int
  | AAdd AddE AddE
  | ABr MulE
  deriving Show

main :: IO ()
main = do
  selfCheck
  print . solve . lines =<< readFile "Day18.input"

solve :: [String] -> Maybe (Int, Int)
solve xs = do
  answer1 <- sum . map eval  <$> traverse decode xs
  answer2 <- sum . map eval2 <$> traverse decode2 xs
  pure (answer1, answer2)

eval :: Expr -> Int
eval = \case
  Num x       -> x
  Br e        -> eval e
  Bin Add a b -> eval a + eval b
  Bin Mul a b -> eval a * eval b

eval2 :: MulE -> Int
eval2 (MulE x xs) = evalA x * product (map evalA xs)
  where
    evalA = \case
      ANum v   -> v
      AAdd a b -> evalA a + evalA b
      ABr e    -> eval2 e

decode :: String -> Maybe Expr
decode = parseMaybe exprP . reverse  -- Ha-ha, cheat!
  where
    exprP :: Parser Expr
    exprP = do
      x <-
        (Num <$> decimal)
        <|> (Br <$> between (char ')') (char '(') exprP)  -- and here!
      (Bin Add x <$> (string " + " *> exprP))
        <|> (Bin Mul x <$> (string " * " *> exprP))
        <|> pure x

decode2 :: String -> Maybe MulE
decode2 = parseMaybe mulP
  where
    mulP :: Parser MulE
    mulP = do
      (x:xs) <- addP `sepBy1` string " * "
      pure $ MulE x xs

    addP :: Parser AddE
    addP = do
      x <-
        (ANum <$> decimal)
        <|> (ABr <$> between (char '(') (char ')') mulP)
      (AAdd x <$> (string " + " *> addP)) <|> pure x

selfCheck :: IO ()
selfCheck = do
  () <$ traverse (check "1" eval decode)
    [ (13632, "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
    , (12240, "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
    ]
  () <$ traverse (check "2" eval2 decode2)
    [ (23340, "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
    , (669060, "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
    ]
  where
    check l e d (v, s) =
      let v' = e <$> d s
      in if not (v' == Just v)
         then putStrLn (l ++ ")" ++ show s ++ " -> " ++ show v')
         else pure ()
