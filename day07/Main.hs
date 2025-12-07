import           Control.Monad.State (State, evalState, get, put)
import qualified Data.List           as List
import qualified Data.List.Split     as Split
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

getAt :: Pos -> [[a]] -> a
getAt (y, x) matrix = matrix !! y !! x

type Pos = (Int, Int)

part2 :: [[Char]] -> Int
part2 (start:input) = evalState (countWorlds (0, startIndex)) Map.empty
  where
    (Just startIndex) = List.elemIndex 'S' start
    endOfInput = length input - 1
    countWorlds :: Pos -> State (Map Pos Int) Int
    countWorlds pos@(y, x) =
      if y == endOfInput
        then pure 1
        else do
          mem <- get
          if pos `Map.member` mem
            then pure $ mem Map.! pos
            else case getAt pos input of
              '.' -> do
                count <- countWorlds (y+1, x)
                mem <- get
                put $ Map.insert pos count mem
                pure count
              '^' -> do
                countLeft <- countWorlds (y+1, x-1)
                countRight <- countWorlds (y+1, x+1)
                let count = countLeft + countRight
                mem <- get
                put $ Map.insert pos count mem
                pure count

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.1.txt"
  putStr "Part 1 : "
  print $ part1 input
  putStr "Part 2 : "
  print $ part2 input
