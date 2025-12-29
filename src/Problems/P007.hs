module Problems.P007 where

import Shared.Methods (nthPrime)

-- Get the n-th prime number
-- One-indexed as per the problem statement
solution :: Integral a => Int -> a
solution = nthPrime . subtract 1
