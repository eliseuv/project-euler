import Methods (interactSolution, readLines)

solution :: [[Int]] -> Int -> [Int]
solution triangle _ = map maximum triangle

main :: IO ()
main = do
  -- Read text file
  strInput <- readFile "euler18.txt"
  -- Parse numbers in triangle
  let triangle = readLines ' ' strInput :: [[Int]]
  -- Interact with solution
  interactSolution $ solution triangle
