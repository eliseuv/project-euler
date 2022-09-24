import Data.Char (isAlpha)

-- Say a given integers name
-- valid from [0..1000]
sayNumber :: Int -> String
sayNumber = sayTuple . decimalTuple
  where
    -- Convert integer into tuple (hudreds, tens, ones)
    decimalTuple :: Int -> (Int, Int, Int)
    decimalTuple x = (xCent, xDec, xUnit)
      where
        xUnit = mod x 10
        xDec = mod (div x 10) 10
        xCent = div x 100
    -- Say name of number represented by the tuple
    sayTuple :: (Int, Int, Int) -> String
    sayTuple (c, d, u) = centWord ++ optAnd ++ decWord ++ unitWord
      where
        -- Units are easy enough
        sayUnit :: Int -> String
        sayUnit u = case u of
          1 -> "one"
          2 -> "two"
          3 -> "three"
          4 -> "four"
          5 -> "five"
          6 -> "six"
          7 -> "seven"
          8 -> "eight"
          9 -> "nine"
          _ -> ""
        -- Since we are going up tu 1000
        centWord = case c of
          10 -> "onethousand"
          0 -> ""
          _ -> sayUnit c ++ "hundred"
        optAnd
          | c `elem` [1 .. 9] && any (/= 0) [d, u] = "and"
          | otherwise = ""
        decWord = case d of
          1 -> case u of
            0 -> "ten"
            1 -> "eleven"
            2 -> "twelve"
            3 -> "thirteen"
            4 -> "fourteen"
            5 -> "fifteen"
            6 -> "sixteen"
            7 -> "seventeen"
            8 -> "eighteen"
            9 -> "nineteen"
            _ -> ""
          2 -> "twenty"
          3 -> "thirty"
          4 -> "forty"
          5 -> "fifty"
          6 -> "sixty"
          7 -> "seventy"
          8 -> "eighty"
          9 -> "ninety"
          _ -> ""
        unitWord
          | all (== 0) [c, d] = case u of
            0 -> "zero"
            _ -> sayUnit u
          | otherwise = case d of
            1 -> ""
            _ -> sayUnit u

solution :: Int
solution = length . concatMap (filter isAlpha . sayNumber) $ [1 .. 1000]

solution' :: [String]
solution' = map sayNumber [1 .. 1000]
