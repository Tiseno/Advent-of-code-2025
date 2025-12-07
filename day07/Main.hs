import           Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State
import qualified Data.List           as List
import           Data.Map            (Map)
import qualified Data.Map            as Map

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

type Pos = (Int, Int)

part2 :: [[Char]] -> Int
part2 (start:input) = evalState (countWorlds (0, startIndex)) Map.empty
  where
    (Just startIndex) = List.elemIndex 'S' start
    endOfInput = length input - 1
    countWorlds :: Pos -> State (Map Pos Int) Int
    countWorlds (y, _) | y == endOfInput = pure 1
    countWorlds pos@(y, x) = do
      memoized <- State.gets (Map.!? pos)
      case memoized of
        Just count -> pure count
        Nothing -> do
          count <- case input !! y !! x of
            '.' -> countWorlds (y+1, x)
            '^' -> do
              countLeft  <- countWorlds (y+1, x-1)
              countRight <- countWorlds (y+1, x+1)
              pure $ countLeft + countRight
          State.modify $ Map.insert pos count
          pure count

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.1.txt"
  putStr "Part 1 : "
  print $ part1 input
  putStr "Part 2 : "
  print $ part2 input
