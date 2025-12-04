topLeft :: Int -> Int -> [Char] -> Int -> Char
topLeft w h text idx
  | idx `mod` w == 0 = '.'
  | idx `div` h == 0 = '.'
  | otherwise = text !! (idx - w - 1)

top :: Int -> Int -> [Char] -> Int -> Char
top w h text idx
  | idx `div` h == 0 = '.'
  | otherwise = text !! (idx - w)

topRight :: Int -> Int -> [Char] -> Int -> Char
topRight w h text idx
  | idx `mod` w == (w - 1) = '.'
  | idx `div` h == 0 = '.'
  | otherwise = text !! (idx - w + 1)

left :: Int -> [Char] -> Int -> Char
left w text idx
  | idx `mod` w == 0 = '.'
  | otherwise = text !! (idx - 1)

right :: Int -> [Char] -> Int -> Char
right w text idx
  | idx `mod` w == (w - 1) = '.'
  | otherwise = text !! (idx + 1)

bottomLeft :: Int -> Int -> [Char] -> Int -> Char
bottomLeft w h text idx
  | idx `mod` w == 0 = '.'
  | idx `div` h == (h - 1) = '.'
  | otherwise = text !! (idx + w - 1)

bottom :: Int -> Int -> [Char] -> Int -> Char
bottom w h text idx
  | idx `div` h == (h - 1) = '.'
  | otherwise = text !! (idx + w)

bottomRight :: Int -> Int -> [Char] -> Int -> Char
bottomRight w h text idx
  | idx `mod` w == (w - 1) = '.'
  | idx `div` h == (h - 1) = '.'
  | otherwise = text !! (idx + w + 1)

neighbors :: Int -> Int -> [Char] -> Int -> [Char]
neighbors w h text idx =
  [ topLeft w h text idx,
    top w h text idx,
    topRight w h text idx,
    left w text idx,
    right w text idx,
    bottomLeft w h text idx,
    bottom w h text idx,
    bottomRight w h text idx
  ]

isFreeRoll :: Int -> Int -> [Char] -> Int -> Bool
isFreeRoll w h text idx
  | text !! idx == '.' = False
  | otherwise = length (filter (/= '.') (neighbors w h text idx)) < 4

part1 :: Int -> Int -> [Char] -> Int
part1 w h text = sum $ map (fromEnum . isFreeRoll w h text) [0 .. length text - 1]

removeRolls :: [Char] -> [Bool] -> [Char]
removeRolls = zipWith (\c f -> (if f then '.' else c))

part2 :: Int -> Int -> [Char] -> [Char]
part2 w h text
  | or freeRolls = part2 w h (removeRolls text freeRolls)
  | otherwise = text
  where
    freeRolls = map (isFreeRoll w h text) [0 .. length text - 1]

main :: IO ()
main = do
  ls <- lines <$> readFile "day4/input.txt"
  let w = length $ head ls
  let h = length ls
  let text = concat ls
  print $ part1 w h text
  print $ length (filter (== '@') text) - length (filter (== '@') (part2 w h text))