module Problems.P018 where

import Shared.Methods (interactSolution, readLines)

solution :: [[Int]] -> Int -> [Int]
solution triangle _ = map maximum triangle

main :: IO ()
main = do
  -- Read text file
  strInput <- readFile "data/P018.txt"
  -- Parse numbers in triangle
  let triangle = readLines ' ' strInput :: [[Int]]
  -- Interact with solution
  interactSolution $ solution triangle
