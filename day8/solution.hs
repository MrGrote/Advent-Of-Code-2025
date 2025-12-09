{- cabal:
build-depends:
  base,
  split ^>=0.2.5,
  containers ^>=0.8
-}

import Data.List (find, sortBy, subsequences)
import Data.List.Split (splitOn)
import Data.Ord (Down (..), comparing)
import Data.Set (Set, fromList, member, singleton, size, toList, union)

euclidianDistance :: (Floating a) => [a] -> [a] -> a
euclidianDistance a b = sqrt $ abs $ sum $ zipWith (\a' b' -> (a' - b') ^ 2) a b

part1 :: (Ord a) => [Set a] -> [(a, a)] -> Int
part1 circuits [] = product $ take 3 $ sortBy (comparing Down) (map size circuits)
part1 circuits pairs =
  let (a, b) = head pairs
      sa =
        case find (member a) circuits of
          Just val -> val
          _ -> error "This should not happen"

      sb =
        case find (member b) circuits of
          Just val -> val
          _ -> error "This should not happen"
   in part1 (union sa sb : [circuit | circuit <- circuits, circuit /= sa && circuit /= sb]) (tail pairs)

part2 :: (Num a, Ord a) => [Set [a]] -> [([a], [a])] -> a
part2 circuits pairs =
  let (a, b) = head pairs
      sa =
        case find (member a) circuits of
          Just val -> val
          _ -> error "This should not happen"

      sb =
        case find (member b) circuits of
          Just val -> val
          _ -> error "This should not happen"
      newTail = [circuit | circuit <- circuits, circuit /= sa && circuit /= sb]
   in if null newTail then head a * head b else part2 (union sa sb : newTail) (tail pairs)

main :: IO ()
main = do
  ls <- lines <$> readFile "day8/input.txt"
  let parsed = map (map read . splitOn ",") ls
  let pairs = fromList $ filter (uncurry (/=)) [(min a b, max a b) | a <- parsed, b <- parsed]
  let sortedPairs = sortBy (\(a, b) (c, d) -> compare (euclidianDistance a b) (euclidianDistance c d)) $ toList pairs
  print $ part1 (map singleton (concatMap (\(a, b) -> [a, b]) (take 1000 sortedPairs))) $ take 1000 sortedPairs
  print $ part2 (map singleton (concatMap (\(a, b) -> [a, b]) sortedPairs)) sortedPairs