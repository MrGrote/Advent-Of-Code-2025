{- cabal:
build-depends:
  base,
  split ^>=0.2.5,
  containers ^>=0.8
-}

import Data.List (sort, sortBy)
import Data.List.Split (splitOn)
import Data.Set (fromList, toList)

rectangleArea :: (Ord a, Num a) => ((a, a), (a, a)) -> a
rectangleArea ((a, b), (c, d)) = (abs (a - c) + 1) * (abs (b - d) + 1)

part1 :: (Ord a, Num a) => [((a, a), (a, a))] -> a
part1 pairs = maximum $ map rectangleArea pairs

pairwise :: [a] -> [(a, a)]
pairwise [a, b] = [(a, b)]
pairwise (a : b : xs) = (a, b) : pairwise (b : xs)

rectangleContainsLineSegment :: (Ord a1, Ord a2) => ((a2, a1), (a2, a1)) -> ((a2, a1), (a2, a1)) -> Bool
rectangleContainsLineSegment ((r1x, r1y), (r2x, r2y)) ((p1x, p1y), (p2x, p2y))
  | p1x == p2x && rxMin < p1x && p1x < rxMax = (pyMin <= ryMin && pyMax > ryMin) || (pyMin < ryMax && pyMax >= ryMax)
  | p1y == p2y && ryMin < p1y && p1y < ryMax = (pxMin <= rxMin && pxMax > rxMin) || (pxMin < rxMax && pxMax >= rxMax)
  | otherwise = False
  where
    [rxMin, rxMax] = sort [r1x, r2x]
    [ryMin, ryMax] = sort [r1y, r2y]
    [pxMin, pxMax] = sort [p1x, p2x]
    [pyMin, pyMax] = sort [p1y, p2y]

part2 :: (Ord a, Num a) => [((a, a), (a, a))] -> [(a, a)] -> a
part2 rectangles points = go sortedPairs
  where
    sortedPairs = sortBy (\a b -> compare (rectangleArea b) (rectangleArea a)) rectangles
    lineSegments = pairwise (last points : points)
    go pairs = if not (any (rectangleContainsLineSegment (head pairs)) lineSegments) then rectangleArea (head pairs) else go (tail pairs)

main :: IO ()
main = do
  ls <- lines <$> readFile "day9/input.txt"
  let parsed = map ((\(a : b : _) -> (a, b)) . map read . splitOn ",") ls
  let rectangles = toList $ fromList $ filter (uncurry (/=)) [(min a b, max a b) | a <- parsed, b <- parsed]
  print $ part1 rectangles
  print $ part2 rectangles parsed