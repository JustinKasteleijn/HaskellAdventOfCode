import Data.List

data RecordType = Safe | Unsafe 
   deriving (Show, Eq)
   
isSafe :: [Integer] -> RecordType
isSafe levels = 
     let diffs = zipWith (-) (tail levels) levels
         allValid = all (\x -> abs x >= 1 && abs x <= 3) diffs
         isIncreasing = all (> 0) diffs
         isDecreasing = all (< 0) diffs
     in if allValid && (isIncreasing || isDecreasing) 
        then Safe 
        else Unsafe
        
solve :: [[Integer]] -> Int
solve = length . filter (==Safe) . map isSafe

main :: IO ()
main = do
  input <- readFile "input.txt" 
  let rows :: [[Integer]] 
      rows = map (map read . words) (lines input)
  print (solve rows)
