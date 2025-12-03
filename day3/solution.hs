import Data.Char (digitToInt)

findJoltage1 :: Int -> Int -> [Char] -> Int
findJoltage1 hi lo (x : xs)
  | null xs && n > lo = hi * 10 + n
  | null xs = hi * 10 + lo
  | n > hi = findJoltage1 n 0 xs
  | n > lo = findJoltage1 hi n xs
  | otherwise = findJoltage1 hi lo xs
  where
    n = digitToInt x

part1 :: IO ()
part1 = do
  input <- readFile "day3/input.txt"
  print $ sum $ findJoltage1 0 0 <$> lines input

findJoltage2 :: [Char] -> Int -> [Char] -> Int
findJoltage2 batteries number bank
  | number == 0 = (read :: String -> Int) batteries
  | otherwise = findJoltage2 (batteries ++ [nextDigit]) (number - 1) (drop 1 (dropWhile (/= nextDigit) bank))
  where
    nextDigit = maximum (take (length bank - number + 1) bank)

part2 :: IO ()
part2 = do
  input <- readFile "day3/input.txt"
  print $ sum $ findJoltage2 [] 12 <$> lines input

main :: IO ()
main = do
  part1
  part2