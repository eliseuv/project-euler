import Methods (factorial, square)

-- Solution is equivalent to finding the number of anagrams of `n` R's and `n` D's
-- S = (2n)!/(n!n!)
solution :: Integral a => a -> a
solution n = div (factorial (2 * n)) (square . factorial $ n)
