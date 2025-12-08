import Data.Function.Memoize (memoize2)
import Data.List (elemIndex, elemIndices, find, tails, transpose)
import Data.Maybe (fromMaybe)

fireFistBeam :: [[Char]] -> [[Char]]
fireFistBeam (x : _ : xs) = map (\n -> if n == 'S' then '|' else '.') x : xs

getNextCharacter :: [Char] -> [Char] -> Char
getNextCharacter (a : b : c : _) (d : e : f : _)
  | e == '^' = '^'
  | d == '^' && a == '|' = '|'
  | f == '^' && c == '|' = '|'
  | b == '|' = '|'
  | otherwise = '.'

windowed :: Int -> [a] -> [[a]]
windowed n xs = filter (\x -> length x == n) (map (take n) (tails xs))

part1 :: Int -> [[Char]] -> Int
part1 n (x : y : xs) = part1 (n + newSplits) (newLine : xs)
  where
    newSplits = sum $ zipWith (\a b -> fromEnum (a == '|' && b == '^')) x y
    xWindow = windowed 3 ('.' : x ++ ".")
    yWindow = windowed 3 ('.' : y ++ ".")
    newLine = zipWith getNextCharacter xWindow yWindow
part1 n (x : xs) = n

part2' :: (Int, Int) -> [[Int]] -> Integer
part2' (x, y) indices =
  case find (> y) (indices !! x) of
    Nothing -> 1
    Just val -> memoizedPart2' (x - 1, val) indices + memoizedPart2' (x + 1, val) indices

memoizedPart2' = memoize2 part2'

part2 :: [[Char]] -> Integer
part2 inp = memoizedPart2' (x, 0) indices
  where
    x = fromMaybe 0 (elemIndex '|' (head inp))
    indices = map (elemIndices '^') (transpose inp)

main :: IO ()
main = do
  ls <- lines <$> readFile "day7/input.txt"
  print $ part1 0 $ fireFistBeam ls
  print $ part2 $ fireFistBeam ls
