import           Debug.Trace
import           System.IO

split :: (Char -> Bool) -> String -> [Int]
split p s =
  case dropWhile p s of
    "" -> []
    s' -> read w : split p s''
      where (w, s'') = break p s'

replace :: (Int, a) -> [a] -> [a]
replace _ [] = []
replace (0, a) (_:xs) = a : xs
replace (n, a) (x:xs) =
  if n < 0
    then x : xs
    else x : replace (n - 1, a) xs

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let numbers = sequence_1202 $ split (== ',') raw
   in case run 0 numbers of
        Left result -> print $ head result
        Right err   -> putStrLn err

run :: Int -> [Int] -> Either [Int] String
run p list
  | opcode == 99 = Left list
  | opcode == 1 =
    run (p + 4) $
    replace (target, x + y) $ trace (debug p opcode x_p x y_p y target) list
  | opcode == 2 =
    run (p + 4) $
    replace (target, x * y) $ trace (debug p opcode x_p x y_p y target) list
  | otherwise = Right $ "Invalid opcode: " ++ show opcode
  where
    opcode = list !! p
    x_p = list !! (p + 1)
    y_p = list !! (p + 2)
    x = list !! x_p
    y = list !! y_p
    target = list !! (p + 3)

sequence_1202 list = replace (2, 2) $ replace (1, 12) list

-- Used for debugging
restoreRaw list = putStrLn $ concatMap (\n -> show n ++ ",") list

debug p opcode x_p x y_p y target =
  show opcode ++
  " at " ++
  show p ++
  " x:" ++
  show x_p ++
  "=" ++ show x ++ " y:" ++ show y_p ++ "=" ++ show y ++ " -> " ++ show target
