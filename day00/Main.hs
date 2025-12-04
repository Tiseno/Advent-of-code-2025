{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
import qualified Data.List.Split as Split

parseInput :: String -> _
parseInput input = lines $ input

part1 :: _ -> _
part1 input = input


main :: IO ()
main = do
  input <- parseInput <$> readFile "input.0.txt"
  putStr "Part 1 : "
  print $ part1 input
