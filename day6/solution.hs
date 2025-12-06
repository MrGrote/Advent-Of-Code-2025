import Data.List (transpose)
import Data.Text (pack, splitOn, unpack)

solveRow :: (Foldable t, Num a) => Char -> t a -> a
solveRow '+' = sum
solveRow '*' = product

part1 :: [[[Char]]] -> Integer
part1 = sum . map (\row -> solveRow (head $ last row) (map read (init row)))

splitOnEmpty :: (Foldable t) => [t Char] -> [[t Char]]
splitOnEmpty [] = []
splitOnEmpty [x] = [[x] | not $ all (== ' ') x]
splitOnEmpty x = start : splitOnEmpty (drop 1 end)
  where
    (start, end) = break (all (== ' ')) x

part2 :: (Num a, Read a) => [[Char]] -> a
part2 rows =
  let operators = filter (/= ' ') $ last rows
      numbers = map (map (read . filter (/= ' '))) (splitOnEmpty (transpose (init rows)))
   in sum $ zipWith solveRow operators numbers

main :: IO ()
main = do
  ls <- lines <$> readFile "day6/input.txt"
  let parsed1 = transpose $ map (map unpack . filter (/= pack "") . splitOn (pack " ") . pack) ls
  print $ part1 parsed1
  print $ part2 ls