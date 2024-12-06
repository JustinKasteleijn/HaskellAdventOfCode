import Data.List
import Data.Maybe

occurCount :: (Eq a) => [a] -> [a] -> Int
occurCount word = 
  length .
  filter (flip elem [word, (reverse word)]) .
  windows (length word)

--Use nothing as padding and transpose the matrix to make the diagonals columns
diagonals :: [[a]] -> [[a]]
diagonals =
  map catMaybes . 
  transpose . 
  zipWith (++) (inits (repeat Nothing)) . 
  (map . map) Just
    
windows :: Int -> [a] -> [[a]]
windows n xs = foldr window [] (tails xs)
   where 
     window l acc 
       | length l >= n = take n l : acc
       | otherwise    = acc

solve1 :: [[Char]] -> Int
solve1 xs =
    sum $
    map
      (occurCount "XMAS")
      ( xs                     
          ++ transpose xs
          ++ diagonals xs
          ++ diagonals (reverse xs)
      )
      
windows2d :: Int -> [[a]] -> [[[a]]]
windows2d n = concatMap (transpose . map (windows n)) . windows n
      
isXMAS3x3 :: [[Char]] -> Bool
isXMAS3x3
    [ [ul, _, ur],
      [_, 'A', _],
      [ll, _, lr]
    ] = all (`elem` ["MS", "SM"]) [[ul, lr], [ur, ll]]
isXMAS3x3 _ = False
   
solve2 :: [[Char]] -> Int 
solve2 = 
   length . 
   filter isXMAS3x3 .
   windows2d 3
       
main :: IO ()
main = do
  input <- readFile "input.txt"
  let parse = words
  print $ solve2 $ parse input
