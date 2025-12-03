import           Data.List as List
import           Data.Map  as Map

type Battery = Char
type Bank = [Battery]
type Joltage = Int

parseInput :: String -> [Bank]
parseInput = lines

allPairs :: [a] -> [(a,a)]
allPairs (x:xs) = ((x,) <$> xs) ++ allPairs xs
allPairs _      = []

maxJoltageFrom2Batteries :: Bank -> Joltage
maxJoltageFrom2Batteries bank =
  maximum $ (\(a,b) -> read [a,b] :: Joltage) <$> allPairs bank

type Mem = Map (Int, [Battery]) (Maybe [Battery])

biggestBatterySequenceOfLength :: Mem -> Int -> [Battery] -> (Mem, Maybe [Battery])
biggestBatterySequenceOfLength m 0 _ = (m, Nothing)
biggestBatterySequenceOfLength m _ [] = (m, Nothing)
biggestBatterySequenceOfLength m n l | Map.member (n, l) m = (m, m Map.! (n, l))
biggestBatterySequenceOfLength m 1 l =
  let biggestBattery = maximum l
   in (Map.insert (1, l) (Just [biggestBattery]) m, Just [biggestBattery])
biggestBatterySequenceOfLength m n l@(x:xs) =
  let (m', nextSequencesMinus1) = biggestBatterySequenceOfLength m (n-1) xs
      (m'', nextSequence) = biggestBatterySequenceOfLength m' n xs
      thisSequence = (x:) <$> nextSequencesMinus1
      biggestSequence = max thisSequence nextSequence -- We can just compare lexicographically, as all sequences are the same length
   in (Map.insert (n, l) biggestSequence m'', biggestSequence)

maxJoltageFromNBatteries :: Int -> Bank -> Joltage
maxJoltageFromNBatteries n bank =
  let (_, Just biggest) = biggestBatterySequenceOfLength Map.empty n bank
   in read biggest

totalMaxJoltage :: (Bank -> Joltage) -> [Bank] -> Joltage
totalMaxJoltage maxJoltageFromBank input = sum $ maxJoltageFromBank <$> input

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.1.txt"
  putStr "Part 1 : "
  print $ totalMaxJoltage maxJoltageFrom2Batteries input
  putStr "Part 1 : "
  print $ totalMaxJoltage (maxJoltageFromNBatteries 2) input
  putStr "Part 2 : "
  print $ totalMaxJoltage (maxJoltageFromNBatteries 12) input
