import qualified Data.List.Split as Split

parseInput :: String -> [(Int, Int)]
parseInput input = parseRange <$> Split.splitOn "," input
  where
    parseRange c = let [start, end] = Split.splitOn "-" c in (read start, read end)

solution :: (Int -> Bool) -> [(Int, Int)] -> Int
solution pred input = sum $ concatMap findInvalid input
  where
    findInvalid (a, b) = filter pred [a..b]

allEqual :: Eq a => [a] -> Bool
allEqual (a:b:xs) | a /= b = False
allEqual (a:b:xs) = allEqual (b:xs)
allEqual _ = True

isInvalid1 :: Int -> Bool
isInvalid1 id =
  let s = show id
   in even (length s) && allEqual (Split.chunksOf (length s `div` 2) s)

isInvalid2 :: Int -> Bool
isInvalid2 id =
  let s = show id
      chunking = (`Split.chunksOf` s) <$> [e | e <- [1..(length s `div` 2)], length s `mod` e == 0]
   in any allEqual chunking

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.1.txt"
  putStr "Part 1 : "
  print $ solution isInvalid1 input
  putStr "Part 2 : "
  print $ solution isInvalid2 input
