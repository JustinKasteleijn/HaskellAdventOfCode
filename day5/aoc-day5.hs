import qualified Data.Set as Set 
import qualified Data.IntMap as Map 
import qualified Data.List as List

type Page = Int
type Rules = Map.IntMap (Set.Set Page)
type Update = [Page]

parseUpdates :: [String] -> [Update]
parseUpdates = map parseUpdate

parseUpdate :: String -> Update
parseUpdate s = map read (splitListOnDelimiter ',' s)

parseRules :: [(Page, Page)] -> Rules
parseRules = foldr go Map.empty
   where go (a, b) set = Map.insertWith Set.union b (Set.singleton a) set

parseRule :: String -> (Page, Page)
parseRule s = 
   let (page1, page2) = splitOnDelimiter '|' s
   in (read page1, read page2)
   
splitListOnDelimiter :: (Eq a) => a -> [a] -> [[a]]
splitListOnDelimiter delimiter = foldr go ([]) 
  where
    go x acc
      | x == delimiter = [] : acc 
      | null acc       = [[x]]      
      | otherwise      = (x : head acc) : tail acc 

splitOnDelimiter :: (Eq a) => a -> [a] -> ([a], [a])
splitOnDelimiter delimiter xs = 
   let (left, right) = break (== delimiter) xs
   in (left, drop 1 right)
   
middlePage :: Update -> Page
middlePage updates = updates !! (length updates `div` 2)
   
invalid :: Rules -> Update -> Bool
invalid rules updates = fst $
                       foldr (updateInvalid rules) (False, Set.empty) updates
                       
updateInvalid :: Rules -> Page -> (Bool, Set.Set Page) -> (Bool, Set.Set Page)
updateInvalid _ _ (True, updates) = (True, updates)
updateInvalid rules update (False, updates) 
   | update `Map.notMember` rules = (False, Set.insert update updates)
   | otherwise = (violates, Set.insert update updates)
   where 
     preceders = rules Map.! update
     violates = not $ Set.null $ Set.intersection preceders updates
     
topoSort :: Rules -> [Page]
topoSort rules = topoSort' rules (Map.keys rules)

topoSort' :: Rules -> [Page] -> [Page]
topoSort' rules nodes = topoSortHelper rules Set.empty nodes []

topoSortHelper :: Rules -> Set.Set Page -> [Page] -> [Page] -> [Page]
topoSortHelper _ _ [] result = result
topoSortHelper rules visited (node:remainingNodes) result
  | node `Set.member` visited = topoSortHelper rules visited remainingNodes result
  | otherwise = 
      let (sorted, remaining) = processNode node rules Set.empty
      in topoSortHelper rules (Set.insert node visited) (remainingNodes ++ remaining) (sorted ++ result)
      
processNode :: Page -> Rules -> Set.Set Page -> ([Page], [Page])
processNode node rules visited =
    let dependencies = Map.findWithDefault Set.empty node rules
        unresolvedDeps = Set.difference dependencies visited
        unresolvedList = Set.toList unresolvedDeps
    in (unresolvedList, unresolvedList)

printable :: Rules -> Set.Set Page -> Page -> Bool
printable rules unprinted page
  | page `Map.notMember` rules = True
  | otherwise = Set.null $ Set.intersection preceders unprinted
  where preceders = rules Map.! page
  
printCandidates :: Rules -> Set.Set Page -> Set.Set Page
printCandidates rules unprinted = 
  Set.filter (printable rules unprinted) unprinted
  
reorder :: Rules -> [Page] -> Set.Set Page -> [Page]
reorder rules printed unprinted 
  | Set.null unprinted = printed
  | otherwise = reorder rules printed' rest
  where candidates = printCandidates rules unprinted
        next = Set.findMin candidates
        rest = Set.delete next unprinted
        printed' = printed ++ [next]
     
solve1 :: Rules -> [Update] -> Int
solve1 rules updates = sum $ 
                       map middlePage (filter (not . (invalid rules)) updates)
                       
solve2 :: Rules -> [Update] -> Int 
solve2 rules updates = sum $
         map middlePage reordered
  where 
     updates' = filter (invalid rules) updates
     pageSets = map Set.fromList updates'
     reordered = map (reorder rules []) pageSets

main :: IO()
main = do
   input <- readFile "input.txt"
   let (rules', updates') = splitOnDelimiter "" (lines input)
   let rules = parseRules (map parseRule rules')
   let updates = parseUpdates updates'
   print $ solve1 rules updates
   print $ solve2 rules updates
