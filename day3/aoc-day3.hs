import Data.List
import Data.Maybe (fromMaybe)
import Distribution.Compat.Prelude (readMaybe)

solve :: [Char]-> Integer
--solve :: [Char] -> [Int]
solve [] = 0
solve (_:[]) = 0
solve (a:b:[]) = 0
solve (a:b:c:[]) = 0
solve (a:b:c:d:[]) = 0
solve (a:b:c:d:xs) = if [a, b, c, d] == "mul("
    then fromIntegral (splitString (take (fromInteger (getIndexOfClosing xs ')')) xs)) + solve xs
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
        fullLength y = foldr(\l acc -> 1 + acc) 0 (map ((\l -> l - 48) . fromEnum) y)
    in  if fullLength secondSubstring > 3 || fullLength substring > 3 || checking substring || checking secondSubstring then 0 else (toNumber substring * toNumber secondSubstring)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print (solve input)
--167650499 correct