import Methods (eachLine, primesBelow)

-- Sum of all primes below a given number
solution :: Integral a => a -> a
-- Easy peasy lemon squeezy
solution = sum . primesBelow

main = interact $ eachLine (\input -> show $ solution (read input :: Integer))
