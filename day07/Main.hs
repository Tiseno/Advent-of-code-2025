import qualified Data.List.Split as Split

parseInput :: String -> [String]
parseInput = lines

countSplits :: String -> String -> Int
countSplits over under = length $ filter isSplit $ zip over under
  where
    isSplit ('|', '^') = True
    isSplit _          = False

propagate :: String -> String -> String
propagate [] [] = []
propagate (_ : '|' : over) (_ : '^' : _ : under) = '|' : '^' : propagate over ('|' : under)
propagate ('|' : over) (_ : under) = '|' : propagate over under
propagate ('S' : over) (_ : under) = '|' : propagate over under
propagate (_ : over) (r : under) = r : propagate over under
propagate over under = error $ "Non exhaustive patterns in propagateToNext. Not matched: over =" ++ show over ++ ", under = " ++ show under

handleLine :: (Int, String) -> String -> (Int, String)
handleLine (count, over) under = (count + countSplits over under, propagate over under)

part1 :: [String] -> Int
part1 (start:input) = fst $ foldl handleLine (0, start) input

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.1.txt"
  putStr "Part 1 : "
  print $ part1 input
