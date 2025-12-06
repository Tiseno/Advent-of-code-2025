import           Data.List (transpose)

type Operator = Int -> Int -> Int
type Problem = (Operator, [Int])
parseInput :: String -> _
parseInput input = parseProblem . reverse <$> transpose (words <$> lines input)
  where
    parseProblem :: [String] -> Problem
    parseProblem (x:xs) = (parseOperator x, fmap read xs)
    parseOperator :: String -> Operator
    parseOperator "*" = (*)
    parseOperator "+" = (+)

part1 :: [Problem] -> Int
part1 input = sum $ fmap (uncurry foldr1) input

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.1.txt"
  putStr "Part 1 : "
  print $ part1 input
