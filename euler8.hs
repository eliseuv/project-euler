import Data.Char (isSpace)
import Methods (interactSolution, maximumAdjacentProduct)

-- Find the maximum adjacent product in a list of digits
solution :: [Int] -> Int -> Int
solution = maximumAdjacentProduct

-- Use solution
main :: IO ()
main = do
  -- Read file
  strInput <- readFile "euler8.txt"
  -- Parse list of digits
  let vals = map (\c -> read [c]) . filter (not . isSpace) $ strInput
  -- Interact with solution
  interactSolution $ solution vals
