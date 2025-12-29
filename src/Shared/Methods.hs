module Shared.Methods (
    -- Tuple utils
    mapTuple,
    toFst,
    toSnd,
    fmapToSnd,
    -- List utils
    uniq,
    findFirst,
    splitString,
    -- Matrix utils
    transpose',
    diagonals,
    -- Simple numerics
    square,
    integralSqrt,
    factorial,
    -- Arbitrary precision integer operation
    integerOperation,
    -- Numerics on lists
    mapMax,
    maximumAdjacentProduct,
    -- Prime numbers
    primes,
    primesBelow,
    nthPrime,
    primeFactors,
    primeFactorsCount,
    -- LCM
    leastCommonMultiplier,
    -- Divisors
    divisorsCount,
    -- Interesting sequences
    pythagoreanTriples,
    triangularNumbers,
    fibonacciNumbers,
    collatzSeq,
    -- Strings
    readLines,
    readDigits,
    -- IO
    eachLine,
    interactSolution,
)
where

import Shared.ComplexInteger (ComplexInteger ((:+)), imagPart, magnitudeSq, realPart)
import Control.Applicative (ZipList (..))
import Control.Arrow ((***))
import Control.Monad (join)
import Data.List (foldl', foldl1', group, scanl', scanl1, transpose)
import Data.Maybe (fromMaybe)
import GHC.List (errorEmptyList)

-- Map but for tuple
-- https://stackoverflow.com/a/9723976
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

toFst :: (a -> b) -> a -> (b, a)
toFst f x = (f x, x)

toSnd :: (a -> b) -> a -> (a, b)
toSnd f x = (x, f x)

fmapToSnd :: (Functor f) => (a -> b) -> f a -> f (a, b)
fmapToSnd = fmap . toSnd

-- Find the first element satisfying a given condition in a given list
-- If no such element is found return Nothing
findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst _ [] = Nothing
findFirst predicate (x : xt)
    | predicate x = Just x
    | otherwise = findFirst predicate xt

-- Ignore repeated sequences in a list
-- Has the effect of keeping only one instance of each element when applied to a sorted list
-- Same as `uniq` unix tool
-- https://hackage.haskell.org/package/Unique-0.4.7.9/docs/Data-List-Unique.html#v:uniq
uniq :: (Eq a) => [a] -> [a]
uniq = map head . group

-- Split a list at every element for which a predicate `p` is true, omitting that element
splitList :: (a -> Bool) -> [a] -> [[a]]
splitList _ [] = []
splitList p xs =
    let (ms, rs) = break p xs
     in case rs of
            [] -> [ms]
            _ -> ms : splitList p (tail rs)

-- Transpose a list of lists
-- https://riptutorial.com/haskell/example/17898/transposing-a-list-of-lists
transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

-- Get diagonals from matrix represented as a list of lists
-- https://hackage.haskell.org/package/universe-base-1.0.2.1/docs/src/Data-Universe-Helpers.html#diagonals
diagonals :: [[a]] -> [[a]]
diagonals = tail . go []
  where
    go b es_ =
        [h | h : _ <- b] : case es_ of
            [] -> transpose ts
            e : es -> go (e : ts) es
      where
        ts = [t | _ : t <- b]

-- Square a number:
-- (\x -> x*x) but cooler
square :: (Num a) => a -> a
square = join (*)

-- Integral square root
integralSqrt :: (Integral a) => a -> a
integralSqrt = floor . sqrt . fromIntegral

-- Factorial
factorial :: (Integral a) => a -> a
factorial n = next n 1
  where
    next 0 f = f
    next n f = next (n - 1) (n * f)

-- Arbitrary precision integer operation
integerOperation :: (Int -> Int) -> String -> String
integerOperation op = concatMap show . reverse . decimalPropagate . reverse . map op . readDigits
  where
    -- Digits propagation part of the sum algorithm
    -- The most significant digits are assumed to be at the front
    decimalPropagate :: [Int] -> [Int]
    decimalPropagate = propagate
      where
        propagate [] = []
        propagate [xf] = [xf]
        propagate (x : xd : xt) = xUnit : decimalPropagate (xd + xDec : xt)
          where
            -- xTens and xUnits are the quotient and the rest, respectively, of x divided by 10
            (xDec, xUnit) = divMod x 10

-- Maximum value of a function when applied to a given list
-- `mapMax f` is equivalent to `maximum . map f` but using `foldl'`
mapMax :: (Ord b) => (a -> b) -> [a] -> b
mapMax f (xh : xt) = foldl' (\y x -> max y (f x)) (f xh) xt
mapMax _ [] = errorEmptyList "mapMax"

-- Find the largest product of `len` adjacent values in a list of integers
maximumAdjacentProduct :: (Integral a) => [a] -> Int -> a
maximumAdjacentProduct xs len = mapMax (maxProd len) subseqs
  where
    -- Generate subsequences without any zeros and discard subsequences that are too short
    subseqs = filter (\ss -> length ss >= len) . splitList (== 0) $ xs
    -- Calculate the maximum product of adjacent values in a list of integers,
    -- assuming the list is long enough and there are no zeros.
    maxProd :: (Integral a) => Int -> [a] -> a
    maxProd len ss = maximum $ scanl' divMult initProd divMultTerms
      where
        -- The initial product is the product of the first `len` values
        initProd = product . take len $ ss
        -- Takes a value, divides by tuple fst and multiply tuple snd
        divMult :: (Integral a) => a -> (a, a) -> a
        divMult p (d, m) = div p d * m
        -- List of tuples of values to divide and multiply
        divMultTerms = zip divVals multVals
          where
            -- List of values to be divided by in order
            divVals =
                let n = length ss
                 in take (n - len) ss
            -- List of values to be multiplied by in order
            multVals = drop len ss

-- Postponed Turner's sieve
-- https://wiki.haskell.org/index.php?title=Prime_numbers#Postponed_Filters
primes :: (Integral a) => [a]
primes = 2 : sieve primes [3, 5 ..]
  where
    sieve :: (Integral a) => [a] -> [a] -> [a]
    sieve ~(p : pt) xs =
        let (xLow, xHigh) = span (< p * p) xs
         in xLow ++ sieve pt [x | x <- xHigh, rem x p /= 0]

-- Get a list with all prime numbers below a given value
primesBelow :: (Integral a) => a -> [a]
primesBelow n = takeWhile (< n) primes

-- Get the n-th prime number
-- Primes are zero-indexed:
-- 0 -> 2, 1 -> 3, 2 -> 5, ...
nthPrime :: (Integral a) => Int -> a
nthPrime = (!!) primes

-- Calculate the prime factors of a given number
-- Optimizations:
--  - The list of primes is generated only once
--  - Checks up to sqrt(n)
-- Can be optimized further:
--  - Do not start over from the first prime number at each iteration
primeFactors :: (Integral a) => a -> [a]
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
            maybeFactor = findFirst ((== 0) . mod n) primesConsidered

-- Count the number of prime factors of given integer
primeFactorsCount :: (Integral a) => a -> Int
primeFactorsCount = length . uniq . primeFactors

-- Least common multiplier of list of integers
-- i.e. the smallest integer that is divisible by all of them
leastCommonMultiplier :: (Integral a) => [a] -> a
leastCommonMultiplier xs = lcmRec 1 primes xs'
  where
    -- Start by removing all 1's in the first call
    xs' = filter (> 1) xs
    -- primesConsidered = (primesBelow . maximum) xs'
    -- Recursive function working with an accumulated product `acc`,
    -- a list of prime numbers `ps` and the values to be tested `xs`
    lcmRec :: (Integral a) => a -> [a] -> [a] -> a
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
        divAny :: (Integral a) => a -> [a] -> Bool
        divAny p = elem 0 . map (`mod` p)
        -- Divide all the numbers in a given list by `p` and filter the non-unitary values
        divFilterOnes :: (Integral a) => a -> [a] -> [a]
        divFilterOnes p = filter (/= 1) . map (\x -> if mod x p == 0 then div x p else x)

-- Number of divisors of a given integer
divisorsCount :: (Integral a) => a -> Int
divisorsCount n = 2 * (divs + 1) + if isPerfectSquare then 1 else 0
  where
    -- The quare root of `n`
    nSqrt :: Double
    nSqrt = sqrt . fromIntegral $ n
    -- Is `n` a perfect square
    isPerfectSquare :: Bool
    isPerfectSquare = nSqrt == (fromIntegral . floor $ nSqrt)
    -- Number of divisors between 2 and sqrt{n}
    divs :: Int
    divs = length . filter (== 0) . map (mod n) $ [2 .. (floor nSqrt - 1)]

-- List all Pythagorean triplets
-- Using method outlined in 3b1b's video: https://www.youtube.com/watch?v=QJYmyhnaaek
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = halfTriples . orderFilter $ [getTriple x y | x <- [2 ..], y <- [1 .. (x - 1)]]
  where
    -- Get Pythagorean triple associated with complex number `x + yi`
    getTriple :: Int -> Int -> (Int, Int, Int)
    getTriple x y = (a, b, c)
      where
        -- Complex number in lattice
        z :: ComplexInteger Int
        z = x :+ y
        -- Complex number squared
        z2 = square z
        -- Real part
        a = realPart z2
        -- Imaginary part
        b = imagPart z2
        -- Hypotenuse
        c = magnitudeSq z
    -- Keep only the triples with `a < b < c`
    orderFilter :: [(Int, Int, Int)] -> [(Int, Int, Int)]
    orderFilter = filter (\(a, b, _) -> a < b)
    -- Half the whole triple if possible
    halfTriples :: [(Int, Int, Int)] -> [(Int, Int, Int)]
    halfTriples [] = error "We somehow ran out of Pythagorean triples!"
    halfTriples (t@(a, b, c) : tt) = (if all even [a, b, c] then (div a 2, div b 2, div c 2) else t) : halfTriples tt

-- Triangular numbers:
-- T_n = \sum_{k=0}^n k
triangularNumbers :: (Integral a) => [a]
triangularNumbers = 0 : next 2 1
  where
    next n t = t : next (n + 1) (t + n)

-- Fibonacci numbers
fibonacciNumbers :: (Integral a) => [a]
fibonacciNumbers = 0 : 1 : zipWith (+) (tail fibonacciNumbers) fibonacciNumbers

-- Collatz sequence
collatzSeq :: (Integral a) => a -> [a]
collatzSeq x = x : sequence x
  where
    sequence :: (Integral a) => a -> [a]
    sequence n = next : sequence next
      where
        next
            | even n = div n 2
            | otherwise = 3 * n + 1

-- Split string at a given separator character
splitString :: Char -> String -> [String]
splitString sep str = case dropWhile (== sep) str of
    "" -> []
    str' -> w : splitString sep str''
      where
        (w, str'') = break (== sep) str'

-- Read string line by line assuming the char `sep` as field separator
-- Returns a list of lines read, each line read being a list of fields
readLines :: (Read a) => Char -> String -> [[a]]
readLines sep = map (map read . splitString sep) . lines

-- Parse string to a list of digits
readDigits :: String -> [Int]
readDigits = map (\c -> read [c])

-- Use to make IO interaction eager
-- Usage: interact $ eachLine
-- https://stackoverflow.com/a/37205614
eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . map f . lines

-- Interact eagerly with a problem solution
interactSolution :: (Read a, Show b) => (a -> b) -> IO ()
interactSolution sol = interact $ eachLine $ show . sol . read
