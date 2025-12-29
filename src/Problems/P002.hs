module Problems.P002 where

-- Project Euler problem 2
-- https://projecteuler.net/problem=2

-- Recurrence relation for the even-valued terms of the Fibonacci sequence:
-- b_n = b_{n-2} + 4 b_{n-1}
-- The first two terms are 2 and 8
fibonacciEven :: [Integer]
fibonacciEven = 2 : 8 : zipWith (\a b -> a + 4 * b) fibonacciEven (tail fibonacciEven)

-- Find the sum of all the even-valued Fibonacci numbers that do not exceed `n`
solution :: Integer -> Integer
solution n = sum $ takeWhile (< n) fibonacciEven
