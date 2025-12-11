import qualified Data.List       as List
import qualified Data.List.Split as Split
import           Data.Map        (Map, (!))
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

type Device = String
type DeviceMapping = Map Device (Set Device)

parseInput :: String -> DeviceMapping
parseInput input = Map.fromList $ parseLine <$> lines input
  where
    parseLine :: String -> (Device, Set Device)
    parseLine line =
      let [input, outputs] = Split.splitOn ": " line
       in (input, Set.fromList $ Split.splitOn " " outputs)

part1 :: DeviceMapping -> Int
part1 deviceMapping = findPaths 0 ["you"]
  where
    findPaths :: Int -> [Device] -> Int
    findPaths count [] = count
    findPaths count positions =
      let (reached, newPositions) = List.partition (== "out") $ allConnected positions
       in findPaths (count + length reached) newPositions
    allConnected :: [Device] -> [Device]
    allConnected = concatMap $ Set.toList . (deviceMapping !)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.1.txt"
  putStr "Part 1 : "
  print $ part1 input
