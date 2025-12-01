rotateDial :: Int -> String -> Int
rotateDial position (dir:xs) =
  let m = if dir == 'R' then 1 else negate 1
   in (position + m * read xs) `mod` 100

main = do
  input <- lines <$> readFile "input.1.txt"
  let result = scanl rotateDial 50 input
  print $ length $ filter (== 0) result
