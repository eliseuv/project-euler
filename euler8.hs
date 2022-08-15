import Data.Char (isSpace)
import Methods (splitString, eachLine)

-- Find the largest product of `len` adjacent digits in a squence of digits provided as a String
solution :: String -> Int -> Int
solution str len = maximum $ map (maxProd len) subseqs
  where
    -- Subsequences of integers to use (drop subsequences smaller than `l`)
    subseqs :: [[Int]]
    subseqs = filter (\ss -> length ss >= len) $ makeSubseqs str
      where
        -- Remove whitespaces, divide into subsequences without zeros and read as list of Ints
        makeSubseqs :: String -> [[Int]]
        makeSubseqs = map (map (\c -> read [c] :: Int)) . splitString '0' . filter (not . isSpace)
    -- Calculate the maximum product for a given subsequence
    maxProd :: Int -> [Int] -> Int
    maxProd len ss = maxProdRec initialProduct ssFront ssBack
      where
        -- The initial product is the product of the first `len` digits
        initialProduct :: Int
        initialProduct = product $ take len ss
        -- List of digits to be divded by in order
        ssFront = take (length ss - len) ss
        -- List of digits to be multiplied by in order
        ssBack = drop len ss
        -- Recursively update product value using the provided lists `front` and `back`,
        -- performing some kind of "moving product"
        maxProdRec :: Int -> [Int] -> [Int] -> Int
        -- Base case: return product when there are no digits in the front or the back
        maxProdRec prod [] [] = prod
        -- Error cases:
        maxProdRec prod [] _ = error "Ran out of front digits!"
        maxProdRec prod _ [] = error "Ran out of back digits!"
        -- Recursive case
        maxProdRec prod (f : ft) (b : bt) = maxProdRec newProd ft bt
          where
            newProd = div prod f * b

main :: IO ()
main = do
  txt <- readFile "euler8.txt"
  interact $ eachLine (\input -> show $ solution txt (read input :: Int))
