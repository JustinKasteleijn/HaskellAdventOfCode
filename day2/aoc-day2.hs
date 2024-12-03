import Data.List

data RecordType = Safe | Unsafe
   deriving (Show, Read, Eq)
   
data Record = Record { 
    levels :: [Integer], 
    safety :: RecordType
} deriving Show



solve :: [[Integer]] -> Integer
solve rows = undefined
  where
  columns = transpose rows
   
parse_line :: String -> Record
parse_line line = Record {
        levels = map read (words line),
        safety = Unsafe
} 

isSafe :: Record -> Record 
isSafe rec = rec { 
                  safety = if (any (\i -> abs (i !! 0 - i !! 1) > 3 || (i !! 0 == i !! 1)) (windows 2 (levels rec)) 
                             || not (isSorted (levels rec)))
                           then Unsafe 
                           else Safe 
             }

windows :: Int -> [a] -> [[a]]
windows size xs
  | size > length xs = []
  | otherwise = take size xs : windows size (tail xs)
  
isSortedAsc :: (Ord a) => [a] -> Bool
isSortedAsc [] = True
isSortedAsc [_] = True
isSortedAsc (x:y:xs) = x <= y && isSortedAsc (y:xs)

isSortedDesc :: (Ord a) => [a] -> Bool
isSortedDesc [] = True
isSortedDesc [_] = True
isSortedDesc (x:y:xs) = x >= y && isSortedDesc (y:xs)

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = isSortedAsc xs || isSortedDesc xs

main :: IO ()
main = do
  input <- readFile "input_test.txt" 
  let t = map (parse_line) (lines input)
  print (map isSafe t)
  print (foldr (\r acc -> if (safety r) == Safe then acc + 1 else acc) 0 (map isSafe t))
