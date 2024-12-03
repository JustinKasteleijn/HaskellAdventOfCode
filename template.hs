import Data.List

solve :: [[Integer]] -> Integer
solve rows = undefined
  where
  columns = transpose rows

main :: IO ()
main = do
  input <- getContents
  let parseLine = map read . words
  let rows = (map parseLine . lines) input
  print (solve rows)