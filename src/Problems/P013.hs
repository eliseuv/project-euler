module Problems.P013 where

import Data.List (intersperse, transpose)
import Shared.Methods (interactSolution, readLines, splitString)

-- Cheated solution using arbitrary precision integers
cheatedSolution :: [String] -> Int -> String
cheatedSolution xs l = take l . show . sum $ parsedNums
  where
    parsedNums = map read xs :: [Integer]

-- Legit solution using bounded integers
legitSolution :: [String] -> Int -> String
legitSolution xs l = take l . concatMap show . reverse . decimalPropagate . reverse . map sum . transpose . parseToMatrix $ xs
  where
    -- Parse list of strings as a matrix of digits, each line representing a single integer
    parseToMatrix :: [String] -> [[Int]]
    parseToMatrix = map (map (\c -> read [c]))
    -- Digits propagation part of the sum algorithm
    -- The most significant digits are assumed to be at the front
    decimalPropagate :: [Int] -> [Int]
    decimalPropagate = propagate
      where
        propagate [] = []
        propagate [xf] = [xf]
        propagate (x : xd : xt) = xUnit : decimalPropagate (xd + xDec : xt)
          where
            -- xTens and xUnits are the quotient and the rest, respectively, of x divided by 10
            (xDec, xUnit) = divMod x 10

main :: IO ()
main = do
  -- Read text file
  strInput <- readFile "data/P013.txt"
  let bigNums = lines strInput
  -- -- Cheated solution with arbitrary precision integers
  -- interactSolution $ cheatedSolution bigNums
  -- Legit solution using bounded integers
  interactSolution $ legitSolution bigNums
