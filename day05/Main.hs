import           Data.Ix         (Ix (inRange, rangeSize))
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

part1 :: [IdRange] -> [Id] -> Int
part1 fresh available =
  length $ filter (\id -> any (`inRange` id) fresh) available

overlaps :: IdRange -> IdRange -> Bool
overlaps (a1, b1) (a2, b2) | a1 < a2 && b2 < b1 || a2 < a1 && b1 < b2 = True
overlaps (a1, b1) (a2, b2) | a1 <= a2 && a2 <= b1 || a1 <= b2 && b2 <= b1 = True
overlaps _ _ = False

merge :: IdRange -> IdRange -> IdRange
merge (a1, b1) (a2, b2) = (min a1 a2, max b1 b2)

toLeftOf :: IdRange -> IdRange -> Bool
toLeftOf (_, b1) (a2, _) = b1 < a2

combineRanges :: IdRange -> [IdRange] -> [IdRange]
combineRanges new [] = [new]
combineRanges new (x : xs) | new `overlaps` x = combineRanges (merge x new) xs
combineRanges new (x : xs) | new `toLeftOf` x = new : x : xs
combineRanges new (x : xs) = x : combineRanges new xs

part2 :: [IdRange] -> Int
part2 fresh = sum $ rangeSize <$> foldr combineRanges [] fresh

main :: IO ()
main = do
  (fresh, available) <- parseInput <$> readFile "input.1.txt"
  putStr "Part 1 : "
  print $ part1 fresh available
  putStr "Part 2 : "
  print $ part2 fresh
