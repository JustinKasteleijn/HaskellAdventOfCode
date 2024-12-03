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


isSafeRecursive :: [Integer] -> Integer -> RecordType
isSafeRecursive levels skip = case isSafe (removeAt skip levels) of 
                                   Safe -> Safe
                                   Unsafe -> if skip >= fromIntegral (length levels)
                                             then Unsafe
                                             else isSafeRecursive levels (skip +1)

 
removeAt :: Integer -> [Integer] -> [Integer]
removeAt n l = case (n, l) of 
                    (_, [])     -> []
                    (0, _)      -> tail l
                    (n, (x:xs)) -> x : (removeAt (n-1) xs)
                    
solve2 :: [[Integer]]  -> Int
solve2 xs = (length . filter (==Safe) . map (\i -> isSafeRecursive i 0)) xs

main :: IO ()
main = do
  input <- readFile "input.txt" 
  let rows :: [[Integer]] 
      rows = map (map read . words) (lines input)
  print (solve2 rows)
