import Data.Char (digitToInt)

findJoltage1 :: [Char] -> Int
findJoltage1 x =
  let hi = maximum $ init x
      rest = drop 1 $ dropWhile (/= hi) x
      lo = maximum rest
   in digitToInt hi * 10 + digitToInt lo

part1 :: IO ()
part1 = do
  input <- readFile "day3/input.txt"
  print $ sum $ findJoltage1 <$> lines input

findJoltage2 :: [Char] -> Int -> [Char] -> Int
findJoltage2 batteries number bank
  | number == 0 = (read :: String -> Int) batteries
  | otherwise =
      let nextDigit = maximum (take (length bank - number + 1) bank)
       in findJoltage2 (batteries ++ [nextDigit]) (number - 1) (drop 1 (dropWhile (/= nextDigit) bank))

part2 :: IO ()
part2 = do
  input <- readFile "day3/input.txt"
  print $ sum $ findJoltage2 [] 12 <$> lines input

main :: IO ()
main = do
  part1
  part2