module Problems.P011 where

import Data.List (foldl')
import Shared.Methods (diagonals, interactSolution, mapMax, maximumAdjacentProduct, readLines, transpose')

-- Find the largest adjacent product for a square grid of integers
-- given as a list of each of its lines `gridLines` of length `len`
-- The adjacent product can be calculated in any of the following directions:
--  - Horizontally
--  - Vertically
--  - Diagonally (positive and negative)
solution :: Integral a => [[a]] -> Int -> a
solution gridLines len = mapMax (mapMax (`maximumAdjacentProduct` len)) [gridLines, gridCols, posDiags, negDiags]
  where
    -- Lists of 1D sequences to look for maximum adjacent product:
    -- The list of grid lines `gridLines` itself.
    -- List of grid columns
    gridCols = transpose' gridLines
    -- List of positive diagonals
    posDiags = dropSmall . diagonals $ gridLines
    -- List of negative diagonals
    negDiags = dropSmall . diagonals . map reverse $ gridLines
    -- Remove lists smaller than the desired adjacent sequence length
    dropSmall :: [[a]] -> [[a]]
    dropSmall = filter (\xs -> length xs >= len)

main :: IO ()
main = do
  -- Read text file
  strInput <- readFile "data/P011.txt"
  -- Parse grid
  let gridLines = readLines ' ' strInput :: [[Int]]
  -- Interact with solution
  interactSolution $ solution gridLines
