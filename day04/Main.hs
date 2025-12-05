import qualified Control.Monad               as Monad
import           Control.Monad.ST            (ST)
import qualified Control.Monad.ST            as ST
import qualified Data.Foldable               as Foldable
import           Data.Maybe                  (mapMaybe)
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

type Diagram = [String]

parseInput1 :: String -> Diagram
parseInput1 = lines

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

type Size = (Int, Int)
type VecMatrix a = (Size, VU.Vector a)

vecMatrixFromListMatrix :: VU.Unbox a => [[a]] -> VecMatrix a
vecMatrixFromListMatrix listMatrix =
  let height = length listMatrix
      width = length $ head listMatrix
      size = height * width
      vec = ST.runST $ do
        v <- VUM.new size
        Monad.forM_ (zip [0..] listMatrix) $ \(y, row) ->
          Monad.forM_ (zip [0..] row) $ \(x, e) ->
            VUM.write v (x + y * width) e
        VU.freeze v
   in ((width, height), vec)

showVecMatrix :: VecMatrix Char -> String
showVecMatrix ((width, height), vec) =
  foldr (\y acc -> showRow y ++ acc ) "" [0..(height - 1)]
  where
    showRow y = foldr (\x acc -> vec VU.! (x + y * width) : acc) "\n" [0..(width - 1)]

parseInput2 :: String -> VecMatrix Char
parseInput2 = vecMatrixFromListMatrix . lines

posToIndex :: Int -> Pos -> Int
posToIndex width (x, y) = x + y * width

indexToPos :: Int -> Int -> Pos
indexToPos width i = (i `mod` width, i `div` width)

indexesInVicinityOf :: Size -> Int -> [Int]
indexesInVicinityOf (width, height) i =
  let (x, y) = indexToPos width i
      positions = filter (\(x, y) -> x >= 0 && y >= 0 && x < width && y < height)
          [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]
   in posToIndex width <$> positions

tryToRemovePaperAtIndex :: Size -> VUM.MVector s Char -> Int -> ST s ()
tryToRemovePaperAtIndex size v i = do
  r <- VUM.read v i
  if r /= '@'
    then pure ()
    else do
      let vicinityIndexes = indexesInVicinityOf size i
      neighbors <- Monad.mapM (VUM.read v) vicinityIndexes
      let neighborPaper = length $ filter (== '@') neighbors
      if neighborPaper >= 4
        then pure ()
        else VUM.write v i '.'

remove :: VecMatrix Char -> VecMatrix Char
remove (size, vec) = ST.runST $ do
  v <- VU.thaw vec
  Monad.forM_ (VU.toList $ VU.elemIndices '@' vec) $ tryToRemovePaperAtIndex size v
  f <- VU.freeze v
  pure (size, f)

removeAll :: VecMatrix Char -> IO (VecMatrix Char)
removeAll mat = do
  putStrLn $ showVecMatrix mat
  let removed = remove mat
  if mat == removed
    then pure removed
    else removeAll removed

part2 :: VecMatrix Char -> IO Int
part2 mat@(_, vec) = do
  let startPapers = length $ VU.toList $ VU.elemIndices '@' vec
  (_, finalState) <- removeAll mat
  let endPapers = length $ VU.toList $ VU.elemIndices '@' finalState
  pure $ startPapers - endPapers

main :: IO ()
main = do
  file <- readFile "input.1.txt"
  putStr "Part 1 : "
  print $ part1 $ parseInput1 file
  putStrLn "Part 2 : "
  result2 <- part2 $ parseInput2 file
  print result2
