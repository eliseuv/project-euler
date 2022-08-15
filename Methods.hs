module Methods
  ( mapTuple,
    findInList,
    square,
    primes,
    primesBelow,
    nthPrime,
    primeFactors,
    leastCommonMultiplier,
    pythagoreanTriplets,
    splitString,
    eachLine,
  )
where

import Control.Arrow ((***))
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import ComplexInteger (ComplexInteger ((:+)), imagPart, magnitudeSq, realPart)

-- Map but for tuple
-- https://stackoverflow.com/a/9723976
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

-- Find the first element satisfying a given condition in a given list
-- If no such element is found return Nothing
findInList :: (a -> Bool) -> [a] -> Maybe a
findInList _ [] = Nothing
findInList cond (x : xt)
  | cond x = Just x
  | otherwise = findInList cond xt

-- Square a number:
-- (\x -> x*x) but cooler
square :: Num a => a -> a
square = join (*)

-- Integral square root
integralSqrt :: Integral a => a -> a
integralSqrt = floor . sqrt . fromIntegral

-- Postponed Turner's sieve
-- https://wiki.haskell.org/index.php?title=Prime_numbers#Postponed_Filters
primes :: Integral a => [a]
primes = 2 : sieve primes [3, 5 ..]
  where
    sieve :: Integral a => [a] -> [a] -> [a]
    sieve ~(p : pt) xs =
      let (xLow, xHigh) = span (< p * p) xs
       in xLow ++ sieve pt [x | x <- xHigh, rem x p /= 0]

-- Get a list with all prime numbers below a given value
primesBelow :: Integral a => a -> [a]
primesBelow n = takeWhile (< n) primes

-- Get the n-th prime number
-- Primes are zero-idexed:
-- 0 -> 2, 1 -> 3, 2 -> 5, ...
nthPrime :: Integral a => Int -> a
nthPrime = (!!) primes

-- Calculate the prime factors of a given number
-- Optimizations:
--  - The list of primes is generated only once
--  - Checks up to sqrt(n)
-- Can be optimized further:
--  - Do not start over from the first prime number at each iteration
primeFactors :: Integral a => a -> [a]
primeFactors n = factorsRec n
  where
    -- Consider only primes below sqrt(n)
    primesConsidered = takeWhile (<= (floor . sqrt . fromIntegral) n) primes
    -- Base case
    factorsRec 1 = []
    -- Recursive case
    factorsRec n = factor : factorsRec (div n factor)
      where
        factor = fromMaybe n maybeFactor
          where
            maybeFactor = findInList ((== 0) . mod n) primesConsidered

-- Least common multiplier of list of integers
-- i.e. the smallest integer that is divisible by all of them
leastCommonMultiplier :: Integral a => [a] -> a
leastCommonMultiplier xs = lcmRec 1 primes xs'
  where
    -- Start by removing all 1's in the first call
    xs' = filter (> 1) xs
    -- primesConsidered = (primesBelow . maximum) xs'
    -- Recursive function working with an accumulated product `acc`,
    -- a list of prime numbers `ps` and the values to be tested `xs`
    lcmRec :: Integral a => a -> [a] -> [a] -> a
    -- Base case: return accumulated product when out of values
    lcmRec acc _ [] = acc
    -- Something went very wrong: we should never run out of primes
    lcmRec acc [] _ = error "We somehow ran out of primes!"
    -- Recursive case
    lcmRec acc ps@(p : pt) xs
      | divAny p xs = lcmRec (acc * p) ps (divFilterOnes p xs)
      | otherwise = lcmRec acc pt xs
      where
        -- Checks `p` divides any number of a given list
        divAny :: Integral a => a -> [a] -> Bool
        divAny p = elem 0 . map (`mod` p)
        -- Divide all the numbers in a given list by `p` and filter the non-unitary values
        divFilterOnes :: Integral a => a -> [a] -> [a]
        divFilterOnes p = filter (/= 1) . map (\x -> if mod x p == 0 then div x p else x)

-- List all Pythagorean triplets
-- Using method outlined in 3b1b's video: https://www.youtube.com/watch?v=QJYmyhnaaek
pythagoreanTriplets :: [(Int, Int, Int)]
pythagoreanTriplets = [getTriplet x y | x <- [2 ..], y <- [1 .. (x - 1)]]
  where
    getTriplet :: Int -> Int -> (Int, Int, Int)
    getTriplet x y = (a, b, c)
      where
        -- Complex number in lattice
        z :: ComplexInteger Int
        z = x :+ y
        -- Complex number squared
        z2 :: ComplexInteger Int
        z2 = square z
        -- Real part
        a :: Int
        a = realPart z2
        -- Imaginary part
        b :: Int
        b = imagPart z2
        -- Hypothenuse
        c :: Int
        c = magnitudeSq z

-- Split string at a given separator character
splitString :: Char -> String -> [String]
splitString sep str = case dropWhile (== sep) str of
  "" -> []
  str' -> w : splitString sep str''
    where
      (w, str'') = break (== sep) str'

-- Use to make IO interaction eager
-- interact $ eachLine
-- https://stackoverflow.com/a/37205614
eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . map f . lines
