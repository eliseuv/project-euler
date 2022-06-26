import Control.Arrow ((***))
import Control.Monad (join)

-- Map function that applies on tuples
-- Taken from https://stackoverflow.com/a/9723976
mapTuple = join (***)

-- Solution to euler1
-- Sum of all multiples of `a` or `b` up to `n`
solution :: Integer -> Integer -> Integer -> Integer
solution n a b = uncurry subtract ((`mapTuple` (last, sum . init)) ($ map sum [mult a n, mult b n, mult (a * b) n]))
  where
    -- List all multiples of `a` up to `n`
    mult :: Integer -> Integer -> [Integer]
    mult a n = [a * k | k <- [1 .. div (n - 1) a]]

-- More obvious and general solution to euler1
-- Sum of all multiples of `as` up to `n`
solution' :: Integer -> [Integer] -> Integer
solution' n as = sum [x | x <- [1 .. n - 1], any (\a -> mod x a == 0) as]

multiples :: Integer -> [Integer] -> [Integer]
multiples n as = [x | x <- [1 .. n - 1], any (\a -> mod x a == 0) as]
