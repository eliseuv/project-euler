import Data.List (maximumBy)
import Data.Ord (comparing)
import Methods (collatzSeq, fmapToSnd)

-- Find the integer below `n` that produces the largest collatz chain
solution :: (Integral a, Num a) => a -> (a, Int)
solution n = maximumBy (comparing snd) (fmapToSnd collatzChainLength [2 .. n - 1])
  where
    collatzChainLength = length . takeWhile (/= 1) . collatzSeq
