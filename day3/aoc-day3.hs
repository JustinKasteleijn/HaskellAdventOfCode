import Data.List
import Data.Maybe (fromMaybe)
import Distribution.Compat.Prelude (readMaybe)

solve :: [Char]-> Integer
--solve :: [Char] -> [Int]
solve [] = 0
solve (_:[]) = 0
solve (a:b:c:d:[]) = 0
solve (a:b:c:d:xs) = if [a, b, c, d] == "mul("
    then fromIntegral (splitString (take (fromInteger (getIndexOfClosing xs ')')) xs)) + solve (drop (fromInteger (getIndexOfClosing xs ')')) xs)
    else solve (b:c:d:xs)

getIndexOfClosing :: [Char] -> Char -> Integer
getIndexOfClosing x y = maybe 0 fromIntegral (elemIndex y x)

splitString :: [Char] -> Int
splitString x = 
    let closingIndex = getIndexOfClosing x ','
        substring = take (fromIntegral closingIndex) x
        secondSubstring = drop (fromIntegral closingIndex + 1) x
        toNumber y = foldl (\acc l -> acc * 10 + l) 0 (map ((\l -> l - 48) . fromEnum) y)
        checking y = foldr (\l acc -> l >= 10 || acc) False (map ((\l -> l - 48) . fromEnum) y)
    in  if checking substring || checking secondSubstring then 0 else toNumber substring * toNumber secondSubstring

main :: IO ()
main = do
  input <- readFile "input.txt"
  --let parseLine = map read . words
  --let rows = (map parseLine . lines) input
  print (splitString "123,456")
  print (solve input)
