import           Data.Ix         (Ix (inRange))
import qualified Data.List.Split as Split

type Id = Int
type IdRange = (Id, Id)

parseInput :: String -> ([IdRange], [Id])
parseInput input =
  let [freshRange, available] = Split.splitOn [""] $ lines input
   in (parseRange <$> freshRange, read <$> available)
  where
    parseRange :: String -> IdRange
    parseRange s = let [a, b] = Split.splitOn "-" s in (read a, read b)

part1 :: ([IdRange], [Id]) -> Int
part1 (fresh, available) =
  length $ filter (\id -> any (`inRange` id) fresh) available

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.1.txt"
  putStr "Part 1 : "
  print $ part1 input
