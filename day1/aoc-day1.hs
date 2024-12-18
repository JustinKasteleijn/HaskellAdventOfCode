import Data.List

data Blood = A | B | AB | O
  deriving(Show, Eq)

instance Ord Blood where 
  compare A  A  = EQ
  compare A  _  = LT
  compare B  B  = EQ
  compare B  A  = GT
  compare B  _  = LT
  compare AB AB = EQ
  compare AB A  = GT
  compare AB B  = GT
  compare AB O  = LT
  compare O  O  = EQ
  compare O  _  = GT
  
solve_part_one :: [[Integer]] -> Integer
solve_part_one rows = let 
       l1 = sort (map head rows)
       l2 = sort (map (!! 1) rows)
       in foldr (\(a, b) acc -> abs(b - a) + acc) 0 (zip l1 l2)

       
solve_part_two :: [[Integer]] -> Integer
solve_part_two rows = let 
       l1 = map head rows
       l2 = map (!! 1) rows
       
       count_occurences :: Integer -> [Integer] -> Integer
       count_occurences x l = fromIntegral $ length $ filter (== x) l
       
       list_builder :: [Integer] -> [Integer] -> [(Integer, [Integer])]
       list_builder l1 l2 = zip l1 (repeat l2)
        
       in foldr (\(x, l) acc -> acc + (count_occurences x l) * x) 0 (list_builder l1 l2)
       

main :: IO ()
main = do
  --input <- readFile "input.txt" 
  --let parseLine = map read . words
  --let rows = (map parseLine . lines) input
  --print (solve_part_two rows)
  let l = [(AB, O), (A, A), (A, B)]
  print (sortOn (snd) l)
