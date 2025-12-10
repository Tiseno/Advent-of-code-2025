import qualified Data.List.Split as Split
import           Data.Set        (Set)
import qualified Data.Set        as Set

type Lights = [Bool]
type ButtonWiring = [Int]
type ButtonWirings = [ButtonWiring]
type JoltageRequirements = [Int]
data Machine = Machine Lights ButtonWirings JoltageRequirements
  deriving (Show)

parseInput :: String -> [Machine]
parseInput input = parseLine . Split.splitOn " " <$> lines input
  where
    parseLine :: [String] -> Machine
    parseLine (lights : rest) =
      Machine (parseDiagram lights) (parseSchematics (init rest)) (parseJoltages (last rest))
    parseDiagram s = (== '#') <$> filter (\c -> c == '.' || c == '#') s
    parseSchematics :: [String] -> [[Int]]
    parseSchematics schematics = (\c -> read ("[" ++ init (tail c) ++ "]")) <$> schematics
    parseJoltages :: String -> [Int]
    parseJoltages c = read ("[" ++ init (tail c) ++ "]")

toggleAt :: Int -> Lights -> Lights
toggleAt i lights = take i lights ++ [not (lights !! i)] ++ drop (i + 1) lights

tryButtonWiring :: Lights -> ButtonWiring -> Lights
tryButtonWiring = foldr toggleAt

analyzeMachine :: Machine -> Int
analyzeMachine (Machine targetLights wirings _) = bfs 0 (Set.fromList [False <$ targetLights])
  where
    bfs :: Int -> Set Lights -> Int
    bfs count lights | targetLights `Set.member` lights = count
    bfs count lights =
      let states = Set.elems lights
          nextStates = concatMap (\state -> tryButtonWiring state <$> wirings) states
       in bfs (count + 1) $ Set.fromList nextStates

part1 :: [Machine] -> Int
part1 input = sum $ analyzeMachine <$> input

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.1.txt"
  putStr "Part 1 : "
  print $ part1 input
