import Methods (interactSolution, maximumAdjacentProduct, readLines, transpose)

solution :: Integral a => [[a]] -> Int -> a
solution gridLines len = maximum . map (maximum . map (`maximumAdjacentProduct` len)) $ [gridLines, gridCols]
  where
    gridCols = transpose gridLines

-- posDiags =

main :: IO ()
main = do
  -- Read text file
  strInput <- readFile "euler11.txt"
  -- Parse grid
  let gridLines = readLines ' ' strInput :: [[Int]]
  -- Interact with solution
  interactSolution $ solution gridLines
