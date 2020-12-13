import Data.Void (Void)
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

main :: IO ()
main = do
  print $ Just (295,1068781) == (solve =<< decode "939\n7,13,x,x,59,x,31,19")
  print . (solve =<<) . decode =<< readFile "Day13.input"

decode :: String -> Maybe (Int, [Maybe Int])
decode = parseMaybe inputP
   where
     inputP = (,) <$> decimal <*> (eol *> idsP <* space)
     idsP = idP `sepBy` char ','
     idP = (Just <$> (decimal :: Parser Int)) <|> (Nothing <$ char 'x')

solve :: (Int, [Maybe Int]) -> Maybe (Integer, Integer)
solve (arrivalTime, buses) = do
  -- part 1
  ((busDeparture, busId) : _) <- pure
    . dropWhile ((< arrivalTime) . fst) . schedule $ catMaybes buses
  let answer1 = fromIntegral $ busId * (busDeparture - arrivalTime)
  -- part 2
  let
    busesI = map fromIntegral $ catMaybes buses
    rems = [ i | (i, Just _) <- zip [0..] buses ]
    time = crt busesI (zipWith (-) busesI rems)
  pure (answer1, time)

schedule :: [Int] -> [(Int, Int)]
schedule []     = []
schedule (x:xs) = foldl' merge (rides x) $ map rides xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

rides :: Int -> [(Int, Int)]
rides n = map (, n) $ [0, n ..]

crt :: [Integer] -> [Integer] -> Integer
crt ns rs = sum (zipWith3 (\n r p -> r * p * invMod p n) ns rs ps) `mod` prod
  where
    prod = product ns
    ps = map (prod `div`) ns

invMod :: Integer -> Integer -> Integer
invMod a m
  | g /= 1    = error "No invMod possible!"
  | otherwise = x `mod` m
  where
    (g, x, _) = egcd a m

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b = (g, x - d * y, y)
  where
    (d, r) = divMod b a
    (g, y, x) = egcd r a
