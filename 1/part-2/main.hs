import           System.IO

main = do
  raw <- readFile "input.txt"
  print $ calc $ parse raw

parse :: String -> [Int]
parse str = map read $ lines str

calc :: [Int] -> Int
calc n = sum $ map (fuelCost 0) n

fuelCost :: Int -> Int -> Int
fuelCost overflow n =
  let v = div n 3 - 2
   in if v <= 0
        then overflow
        else fuelCost (overflow + v) v
