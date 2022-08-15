import Data.List (find)
import Data.Maybe (fromMaybe)
import Methods (pythagoreanTriples)

-- Find a Pythagorean triple that sums to `n` and return its product
solution :: Int -> ((Int, Int, Int), Int)
solution n = (triple, prod)
  where
    triple = fromMaybe (0, 0, 0) $ find (\(a, b, c) -> a + b + c == n) pythagoreanTriples
    prod = (\(a, b, c) -> a * b * c) triple
