module Problems.P012 where

import Shared.Methods (divisorsCount, findFirst, fmapToSnd, interactSolution, triangularNumbers)

solution :: Integral a => Int -> Maybe (a, Int)
solution k = findFirst ((> k) . snd) . fmapToSnd divisorsCount $ triangularNumbers

main :: IO ()
main = do
  -- Interact with solution
  interactSolution solution
