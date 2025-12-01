rotateDial1 :: Int -> String -> Int
rotateDial1 position (dir:distance) =
  let m = if dir == 'R' then 1 else negate 1
      turn = m * read distance
   in (position + turn) `mod` 100

part1 :: [String] -> Int
part1 input = length $ filter (== 0) $ scanl rotateDial1 50 input

rotateDial2 :: (Int, Int) -> String -> (Int, Int)
rotateDial2 (position, count) (dir:distance) =
  let m = if dir == 'R' then 1 else negate 1
      turn = m * read distance
      fullRotations = abs $ turn `quot` 100
      extraDistance = turn `rem` 100
      newPosition = position + extraDistance
   in (newPosition `mod` 100, count + fullRotations + (if position /= 0 && (newPosition <= 0 || newPosition >= 100) then 1 else 0))

part2 :: [String] -> Int
part2 input = snd $ foldl (\(pos, count) rot -> rotateDial2 (pos, count) rot) (50, 0) input

main :: IO ()
main = do
  input <- lines <$> readFile "input.1.txt"
  putStr "Part 1 : "
  print $ part1 input
  putStr "Part 2 : "
  print $ part2 input
