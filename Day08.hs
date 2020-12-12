{-# LANGUAGE NamedFieldPuns #-}

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Text.Read (readMaybe)

data Op
  = Nop
  | Jmp
  | Acc

type Cmd = (Op, Int)

data Machine = Machine
  { acc :: !Int
  , pc  :: !Int
  , pcs :: !IntSet
  , end :: !Int
  , pgm :: !(IntMap Cmd)
  }

data Result
  = Success
  | Loop
  | PCOverflow

main :: IO ()
main = print . solve . map decode . lines =<< readFile "Day08.input"

solve :: [Cmd] -> (Int, Int)
solve p =
  ( acc . snd . run $ initMachine p
  , tryToFix p
  )

initMachine :: [Cmd] -> Machine
initMachine p = Machine 0 0 Set.empty (length p) . Map.fromList $ zip [0..] p

run :: Machine -> (Result, Machine)
run !(m@(Machine {acc, pc, pcs, end, pgm}))
  | pc `Set.member` pcs = (Loop, m)
  | pc == end           = (Success, m)
  | otherwise           =
    case Map.lookup pc pgm of
      Nothing  -> (PCOverflow, m)
      Just cmd ->
        let mm = m{pcs = Set.insert pc pcs}
        in case cmd of
          (Nop, _) -> run mm{pc = pc + 1}
          (Acc, x) -> run mm{pc = pc + 1, acc = acc + x}
          (Jmp, x) -> run mm{pc = pc + x}

tryToFix :: [Cmd] -> Int
tryToFix p = case dropWhile invalid $ map (run . initMachine) (variantsOf p) of
  ((_, m) : _) -> acc m
  _            -> error $ "Can't fix this program :("
  where
    invalid = \case
      (Success, _) -> False
      _            -> True

    variantsOf = \case
      []                 -> [[]]
      (op@(Acc, _) : xs) -> map (op :) (variantsOf xs)
      (op          : xs) ->
        (toggle op : xs) :  map (op :) (variantsOf xs)

    toggle (Nop, v) = (Jmp, v)
    toggle (_, v)   = (Nop, v)

decode :: String -> Cmd
decode s =
  case (,) <$> op <*> arg of
    Just x  -> x
    Nothing -> error $ "Bad line: " ++ s
  where
    arg = readMaybe . dropWhile (== '+') $ drop 4 s
    op = case (take 3 s) of
      "nop" -> Just Nop
      "jmp" -> Just Jmp
      "acc" -> Just Acc
      _     -> Nothing
