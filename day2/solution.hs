import Data.Text (pack, splitOn, unpack)

parse :: IO [[Integer]]
parse = do
  input <- readFile "day2/input.txt"
  return $ map (\x -> read $ unpack x :: Integer) . splitOn (pack "-") <$> splitOn (pack ",") (pack input)

isInvalid1 :: (Show a) => a -> Bool
isInvalid1 n = drop (length (show n) `div` 2) (show n) == take (length (show n) `div` 2) (show n)

part1 :: IO ()
part1 = do
  input <- parse
  print $ sum $ concatMap (filter isInvalid1 . (\x -> [head x .. last x])) input

isRepeating :: (Eq a) => [a] -> Int -> Bool
isRepeating s n
  | length s `mod` n /= 0 = False
  | length s == n = True
  | take n s == take n (drop n s) = isRepeating (drop n s) n
  | otherwise = False

isInvalid2 :: (Show a) => a -> Bool
isInvalid2 n = any (isRepeating (show n)) [1 .. (length (show n) `div` 2)]

part2 :: IO ()
part2 = do
  input <- parse
  print $ sum $ concatMap (filter isInvalid2 . (\x -> [head x .. last x])) input

main :: IO ()
main = do
  part1
  part2