import Methods (divisorsCount, findInList, fmapToSnd, interactSolution, triangularNumbers)

solution :: Integral a => Int -> Maybe (a, Int)
solution k = findInList ((> k) . snd) . fmapToSnd divisorsCount $ triangularNumbers

main :: IO ()
main = do
  -- Interact with solution
  interactSolution solution
