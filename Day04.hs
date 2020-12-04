{-# LANGUAGE LambdaCase #-}

import Data.Char (isDigit)
import Data.List (sort)
import Text.Read (readMaybe)

type Field = String
type Value = String
type Validator = String -> Bool
type Passport = [(Field, Value)]

main :: IO ()
main = do
  print
    $ all isValid (decode validExamples)
    && not (any isValid $ decode invalidExamples)
  print . solve . decode . lines =<< readFile "Day04.input"

decode :: [String] -> [Passport]
decode = go []
  where
    go acc []        = [toFields acc]
    go acc ("" : xs) = toFields acc : go [] xs
    go acc (x  : xs) = go (acc ++ ' ' : x) xs
    toFields = map toField . words
    toField = (,) <$> take 3 <*> drop 4

solve :: [Passport] -> (Int, Int)
solve ps = (length lookRight, length $ filter isValid lookRight)
  where
    lookRight = filter hasAllFields ps

    hasAllFields = (== significantFields rules) . significantFields

isValid :: Passport -> Bool
isValid fields = all checked rules
  where
    fields' = ("cid", "") : fields
    checked (f, r) = case lookup f fields' of
      Just v -> r v
      _      -> False

significantFields :: [(Field, a)] -> [Field]
significantFields = sort .  filter (/= "cid") . map fst

rules :: [(Field, Validator)]
rules =
  [ ("byr", intInRange 1920 2002)
  , ("iyr", intInRange 2010 2020)
  , ("eyr", intInRange 2020 2030)
  , ("hgt", validHeight)
  , ("hcl", validHairColor)
  , ("ecl", validEyeColor)
  , ("pid", validPId)
  , ("cid", const True)
  ]

intInRange :: Int -> Int -> Validator
intInRange f t s = case readMaybe s of
  Just x -> f <= x && x <= t
  _      -> False

validHeight :: Validator
validHeight s = validCm || validIn
  where
    validCm = suffix == "cm" && intInRange 150 193 prefix
    validIn = suffix == "in" && intInRange 59 76 prefix
    (prefix, suffix) = splitAt (length s - 2) s

validHairColor :: Validator
validHairColor = \case
  ('#':xs) -> length xs == 6 && all isLCHexDigit xs
  _        -> False
  where
    isLCHexDigit x = isDigit x || x `elem` "abcdef"

validEyeColor :: Validator
validEyeColor =
  (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

validPId :: Validator
validPId = (&&) <$> ((== 9) . length) <*> all isDigit

invalidExamples :: [String]
invalidExamples =
  [ "eyr:1972 cid:100"
  , "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
  , ""
  , "iyr:2019"
  , "hcl:#602927 eyr:1967 hgt:170cm"
  , "ecl:grn pid:012533040 byr:1946"
  , ""
  , "hcl:dab227 iyr:2012"
  , "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
  , ""
  , "hgt:59cm ecl:zzz"
  , "eyr:2038 hcl:74454a iyr:2023"
  , "pid:3556412378 byr:2007"
  ]

validExamples :: [String]
validExamples =
  [ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
  , "hcl:#623a2f"
  , ""
  , "eyr:2029 ecl:blu cid:129 byr:1989"
  , "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
  , ""
  , "hcl:#888785"
  , "hgt:164cm byr:2001 iyr:2015 cid:88"
  , "pid:545766238 ecl:hzl"
  , "eyr:2022"
  , ""
  , "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
  ]
