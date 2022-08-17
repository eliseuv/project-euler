import Methods (findInList, fmapToSnd, interactSolution, triangularNumbers)

-- solution :: Integral a => Int -> Maybe (a, Int)
solution k = findInList ((> (k - 2)) . snd) . fmapToSnd (length . divisors) $ tail triangularNumbers
  where
    divisors :: Integral a => a -> [a]
    divisors n = filter (\x -> mod n x == 0) [2 .. div n 2]

main :: IO ()
main = do
  -- Interact with solution
  interactSolution solution
