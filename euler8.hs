import Data.Char (isSpace)
import Methods (interactSolution, maximumAdjacentProduct)

-- Find the maximum adjacent product in a list of digits
solution :: [Int] -> Int -> Int
solution = maximumAdjacentProduct

-- Parse string to list of digits
parseDigitsList :: Read a => String -> [a]
parseDigitsList = map (\c -> read [c]) . filter (not . isSpace)

-- Use solution
main :: IO ()
main = do
  -- Read file
  strInput <- readFile "euler8.txt"
  -- Parse list of digits
  let vals = parseDigitsList strInput
  -- Interact with solution
  interactSolution $ solution vals
