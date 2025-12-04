import           Data.Maybe (mapMaybe)

type Diagram = [String]

parseInput :: String -> Diagram
parseInput = lines

-- I dont have !? apparently
atP :: [[a]] -> (Int, Int) -> Maybe a
atP l (y, x) | x < 0 || y < 0 || y >= length l = Nothing
atP l (y, x) =
  let row = l !! y
   in if x >= length row
        then Nothing
        else Just $ row !! x

type Pos = (Int, Int)

isAccessiblePaper :: Diagram -> Pos -> Bool
isAccessiblePaper diagram (y, x) | diagram !! y !! x /= '@' = False
isAccessiblePaper diagram (y, x) =
  let neighborCoords = [(y + dy, x + dx) | dy <- [-1 .. 1], dx <- [-1 .. 1], (dy /= 0) || (dx /= 0)]
      neighboringPaper = length $ filter (== '@') $ mapMaybe (atP diagram) neighborCoords
   in neighboringPaper < 4

part1 :: Diagram -> Int
part1 input = length $ filter id $ isAccessiblePaper input <$> [(y, x) | y <- [0 .. length input - 1], x <- [0 .. length (head input) - 1]]

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.1.txt"
  putStr "Part 1 : "
  print $ part1 input
