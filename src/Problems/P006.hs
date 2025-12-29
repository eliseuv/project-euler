module Problems.P006 where

import Control.Monad (join)
import Shared.Methods (mapTuple)

-- Difference between the square of the sum and the sum of the squares of a given list of numbers
solution :: Num a => [a] -> a
solution = uncurry subtract . (`mapTuple` (sum . map square, square . sum)) . flip ($)
  where
    -- More point-free goodness :)
    square = join (*)
