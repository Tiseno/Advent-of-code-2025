import qualified Data.Char       as Char
import           Data.List       as List
import qualified Data.List.Split as Split

type Operator = Int -> Int -> Int
type Problem = (Operator, [Int])

parseOperator :: String -> Operator
parseOperator "*" = (*)
parseOperator "+" = (+)

solve :: (String -> [Problem]) -> String -> Int
solve parseProblems input = sum (uncurry foldr1 <$> parseProblems input)

parse1 :: String -> [Problem]
parse1 input = parseProblem . reverse <$> List.transpose (words <$> lines input)
  where
    parseProblem :: [String] -> Problem
    parseProblem (x:xs) = (parseOperator x, read <$> xs)

parse2 :: String -> [Problem]
parse2 input = fmap parseProblem $ Split.splitWhen (all (== ' ')) $ List.transpose $ lines input
  where
    parseProblem :: [String] -> Problem
    parseProblem (x:xs) =
      let (num, op) = span (\c -> Char.isDigit c || Char.isSpace c) x
       in (parseOperator op, read <$> (num:xs))

main :: IO ()
main = do
  input <- readFile "input.1.txt"
  putStr "Part 1 : "
  print $ solve parse1 input
  putStr "Part 2 : "
  print $ solve parse2 input
