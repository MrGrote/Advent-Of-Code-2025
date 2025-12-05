import Data.List (sort)

parseRange :: [Char] -> (Int, Int)
parseRange range =
  let start = read $ takeWhile (/= '-') range
      end = read $ tail $ dropWhile (/= '-') range
   in (start, end)

isBetween :: (Ord a) => a -> (a, a) -> Bool
isBetween n (lo, hi) = (lo <= n) && (n <= hi)

isFresh :: (Foldable t, Ord a) => a -> t (a, a) -> Bool
isFresh n = any (isBetween n)

part1 :: (Foldable t, Ord a) => [a] -> t (a, a) -> Int
part1 ingredients ranges = length $ filter (`isFresh` ranges) ingredients

collapseRanges :: (Ord b) => [(b, b)] -> [(b, b)]
collapseRanges [] = []
collapseRanges [x] = [x]
collapseRanges ((lo, hi) : xs)
  | nlo <= hi = collapseRanges ((lo, max nhi hi) : tail xs)
  | otherwise = (lo, hi) : collapseRanges xs
  where
    (nlo, nhi) = head xs

rangeSize :: (Num a) => (a, a) -> a
rangeSize (lo, hi) = hi - lo + 1

part2 :: (Foldable t, Num a, Ord a) => t a -> [(a, a)] -> a
part2 ingredients ranges = sum $ map rangeSize $ collapseRanges $ sort ranges

main :: IO ()
main = do
  ls <- lines <$> readFile "day5/input.txt"
  let ranges = parseRange <$> takeWhile (/= "") ls
  let ingredients = map read $ tail $ dropWhile (/= "") ls
  print $ part1 ingredients ranges
  print $ part2 ingredients ranges