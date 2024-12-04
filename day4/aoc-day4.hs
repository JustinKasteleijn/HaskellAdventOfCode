import Data.List
 
getElement :: [[Char]] -> Int -> Int -> Maybe Char
getElement grid row col =
    if row < length grid
    then case (grid !! row) of
        r -> if col < length r then Just (r !! col) else Nothing
    else Nothing

     
countHorizontal :: [Char] -> Integer
countHorizontal xs = foldr countMatches 0 (tails xs)
  where
    countMatches sublist acc
      | "XMAS" `isPrefixOf` sublist = acc + 1
      | "SAMX" `isPrefixOf` sublist = acc + 1
      | otherwise = acc

main :: IO ()
main = do
  input <- readFile "input_test.txt"
  let parse = words
  print $ countHorizontal "SAMXXXMASM"
