type Bank = String
type Joltage = Int

parseInput :: String -> [Bank]
parseInput = lines

allPairs :: [a] -> [(a,a)]
allPairs (x:xs) = ((x,) <$> xs) ++ allPairs xs
allPairs _      = []

part1 :: [Bank] -> Joltage
part1 input = sum $ calcualteMaxJoltage <$> input
  where
    calcualteMaxJoltage :: Bank -> Joltage
    calcualteMaxJoltage bank =
      maximum $ (\(a,b) -> read [a,b] :: Joltage) <$> allPairs bank

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.1.txt"
  putStr "Part 1 : "
  print $ part1 input
