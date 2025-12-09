import qualified Data.List.Split as Split

parseInput :: String -> [(Int, Int)]
parseInput input = parseLine <$> lines input
  where
    parseLine :: String -> (Int, Int)
    parseLine l =
      let [a, b] = Split.splitOn "," l
       in (read a, read b)

allPairs :: [a] -> [(a, a)]
allPairs []       = []
allPairs (x : xs) = ((x,) <$> xs) ++ allPairs xs

part1 :: [(Int, Int)] -> Int
part1 input =
  maximum $ (\((a1, b1), (a2, b2)) -> (1 + abs (a1 - a2)) * (1 + abs (b1 - b2))) <$> allPairs input

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.1.txt"
  putStr "Part 1 : "
  print $ part1 input
