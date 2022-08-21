import Methods (integerOperation, readDigits)

-- Cheated solution using arbitrary precision integers
cheatedSolution :: Int -> Int
cheatedSolution n = sum . readDigits . show $ 2 ^ n

-- Legit solution
legitSolution :: Int -> Int
legitSolution n = sum . readDigits . (!! (n - 1)) . iterate (integerOperation (* 2)) $ "2"
