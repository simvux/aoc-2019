import           System.IO

main = do
  raw <- readFile "input.txt"
  print $ calc $ parse raw

parse :: String -> [Int]
parse str = map read $ lines str

calc :: [Int] -> Int
calc n = sum $ map (\n -> div n 3 - 2) n
