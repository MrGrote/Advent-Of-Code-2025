parseInstruction :: String -> Integer
parseInstruction (x : xs)
  | x == 'L' = -read xs :: Integer
  | x == 'R' = read xs :: Integer

rotate :: (Integral a, Num b) => (a, b) -> a -> (a, b)
rotate (dial, zeros) distance
  | newDial `mod` 100 == 0 = (newDial, zeros + 1)
  | newDial `mod` 100 /= 0 = (newDial, zeros)
  where
    newDial = dial + distance

part1 :: IO ()
part1 = do
  input <- readFile "day1/input.txt"
  let instructions = parseInstruction <$> lines input
  let result = foldl rotate (50, 0) instructions
  print result

rotate2 :: (Num a1, Integral a2, Ord a1) => (a2, Int) -> a1 -> (a2, Int)
rotate2 (dial, zeros) distance
  | distance < 0 = rotate2 (dial - 1, zeros + fromEnum (((dial - 1) `mod` 100) == 0)) (distance + 1)
  | distance > 0 = rotate2 (dial + 1, zeros + fromEnum (((dial + 1) `mod` 100) == 0)) (distance - 1)
  | otherwise = (dial, zeros)

part2 :: IO ()
part2 = do
  input <- readFile "day1/input.txt"
  let instructions = parseInstruction <$> lines input
  let result = foldl rotate2 (50, 0) instructions
  print result

main :: IO ()
main = do
  part1
  part2