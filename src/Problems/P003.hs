module Problems.P003 where

import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)

-- Given a list of the first prime numbers, determine the next prime number
-- This function is more efficient if the list of primes is in decreasing order
nextPrime :: [Integer] -> Integer
-- The first prime number is `2`
nextPrime [] = 2
-- The next prime numbers are determined using Eratosthenes prime number siege
-- `fromJust` here is justified because there demonstrably infinite prime numbers
nextPrime ps = fromJust $ find notDivisible [(head ps + 1) ..]
  where
    notDivisible :: Integer -> Bool
    notDivisible x = 0 `notElem` map (mod x) ps

-- First `n` prime numbers
firstPrimes :: Int -> [Integer]
firstPrimes 0 = []
firstPrimes n = nextPrime prevPrimes : prevPrimes
  where
    prevPrimes :: [Integer]
    prevPrimes = firstPrimes (n - 1)

-- https://wiki.haskell.org/index.php?title=Prime_numbers

-- Naive Turner's sieve
-- https://wiki.haskell.org/index.php?title=Prime_numbers#Turner.27s_sieve_-_Trial_division
primesTurnerNaive :: Integral a => [a]
primesTurnerNaive = sieve [2 ..]
  where
    sieve :: Integral a => [a] -> [a]
    sieve ~(p : xs) = p : sieve [x | x <- xs, rem x p /= 0]

-- Postponed Turner's sieve
-- https://wiki.haskell.org/index.php?title=Prime_numbers#Postponed_Filters
primesTurner :: Integral a => [a]
primesTurner = 2 : sieve primesTurner [3, 5 ..]
  where
    sieve :: Integral a => [a] -> [a] -> [a]
    sieve ~(p : pt) xs =
      let (xLow, xHigh) = span (< p * p) xs
       in xLow ++ sieve pt [x | x <- xHigh, rem x p /= 0]

-- Algorithm to use to generate primes
primes :: Integral a => [a]
primes = primesTurner

-- Calculate the prime factors of a given number
-- Can be optimized further:
--  - Check up to sqrt(n)
--  - Do not start over from the first prime number at each iteration
factorsNaive :: Integral a => a -> [a]
factorsNaive 1 = []
factorsNaive n = factor : factorsNaive (div n factor)
  where
    factor = fromJust $ find ((== 0) . mod n) primes

-- Calculate the prime factors of a given number
-- Optimizations:
--  - The list of primes is generated only once
--  - Checks up to sqrt(n)
-- Can be optimized further:
--  - Do not start over from the first prime number at each iteration
factorsOpt :: Integral a => a -> [a]
factorsOpt n = factorsRec n
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
            maybeFactor = find ((== 0) . mod n) primesConsidered

-- Select algorithm to calculate prime factors
factors :: Integral a => a -> [a]
factors = factorsOpt

-- Find the largest prime factor of a given number
solution :: Integer -> Integer
solution = maximum . factors
