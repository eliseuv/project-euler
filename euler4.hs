import Data.Char (digitToInt)
import Data.List (find)
import Data.Maybe (fromMaybe)

-- Tuple form type
type TupleForm = (String, Maybe Char, String)

-- Test if a given tuple form is a palindrome
isPalindrome :: TupleForm -> Bool
isPalindrome (beg, _, end) = beg == reverse end

-- Convert to tuple form
toTupleForm :: Show a => a -> TupleForm
toTupleForm x = (xBegin, xMid, xEnd)
  where
    xStr = show x
    l = div (length xStr) 2
    xBegin = take l xStr
    xMid = middle xStr
      where
        middle [] = Nothing
        middle [a] = Just a
        middle lst = (middle . init . tail) lst
    xEnd = drop (length xStr - l) xStr

-- Retrieve from tuple form
fromTupleForm :: Read a => TupleForm -> a
fromTupleForm (beg, mid, end) = read $ beg ++ m ++ end
  where
    m = case mid of
      Nothing -> ""
      Just c -> [c]

-- Calulates the largest integer palindrome integer below a given integer value
largestIntegerPalindromeBelow :: Integer -> Integer
largestIntegerPalindromeBelow n = fromTupleForm resultTuple
  where
    resultTuple =
      let nTuple = toTupleForm n
       in case nTuple of
            (_, Nothing, _) -> evenDigits nTuple
            _ -> oddDigits nTuple
      where
        readDecOne :: String -> String
        readDecOne s = show ((read s :: Integer) - 1)
        isPowerOfTen :: String -> Bool
        isPowerOfTen str = (head str == '1') && all (== '0') (tail str)
        evenDigits :: TupleForm -> TupleForm
        evenDigits (lhs, Nothing, rhs)
          | (read lhs :: Integer) < (read rhs :: Integer) = (lhs, Nothing, reverse lhs)
          | isPowerOfTen lhs = powerOfTen lhs
          | otherwise =
            let lhs' = show ((read lhs :: Integer) - 1)
             in (lhs', Nothing, reverse lhs')
          where
            powerOfTen :: String -> TupleForm
            powerOfTen lhs = (nines, Just '9', nines)
              where
                nines = tail $ map (const '9') lhs
        oddDigits :: TupleForm -> TupleForm
        oddDigits (lhs, Just m, rhs)
          | null lhs && null rhs = oneDigit m
          | (read lhs :: Integer) < (read rhs :: Integer) = (lhs, Just m, reverse lhs)
          | otherwise = case m of
            '0' -> midIsZero lhs
            _ -> (lhs, Just (head $ readDecOne [m]), reverse lhs)
          where
            oneDigit :: Char -> TupleForm
            oneDigit n
              | n == '0' = ("", Nothing, "") -- error "Zero is the first natural number!"
              | otherwise =
                let n' = head $ show (digitToInt n - 1)
                 in ("", Just n', "")
            midIsZero :: String -> TupleForm
            midIsZero lhs
              | isPowerOfTen lhs = (nines, Nothing, nines)
              | otherwise = (lhs', Just '9', reverse lhs')
              where
                nines = map (const '9') lhs
                lhs' = readDecOne lhs

-- Calculate the largest palindrome that is a product of two numbers with a given number of digits
largestPalindromeProduct :: Integer -> (Integer, Integer, Integer)
largestPalindromeProduct digits = (pal, k1, k2)
  where
    (pal, k1) = fromMaybe (0, 0) $ find (\(p, k) -> mod p k == 0) [(p, k) | p <- palindromesList, k <- integersList p]
      where
        palindromesList :: [Integer]
        palindromesList = iterate largestIntegerPalindromeBelow itBegin
          where
            itBegin = largestIntegerPalindromeBelow (nines * nines)
        integersList :: Integer -> [Integer]
        integersList p = [nines, nines - 1 .. floor $ sqrt $ fromIntegral p]
        nines = read (map (const '9') [1 .. digits]) :: Integer
    k2 = div pal k1
