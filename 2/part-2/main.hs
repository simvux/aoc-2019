{-# LANGUAGE LambdaCase #-}

import           Data.List
import           Data.Maybe
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
  let numbers = split (== ',') raw
   in handle $
      find (not . null) $
      map
        (\n2 -> mapMaybe (\n1 -> tryForOutput (n1, n2) numbers) [0 .. 99])
        [0 .. 99]
  where
    handle =
      \case
        Just found -> view $ head found
        Nothing -> putStrLn "not found"
    view (noun, verb) =
      putStrLn $
      show noun ++ " * " ++ show verb ++ " = " ++ show (100 * noun + verb)

run :: Int -> [Int] -> Either [Int] String
run p list
  | opcode == 99 = Left list
  | opcode == 1 = run (p + 4) $ replace (target, x + y) list
  | opcode == 2 = run (p + 4) $ replace (target, x * y) list
  | otherwise = Right $ "Invalid opcode: " ++ show opcode
  where
    opcode = list !! p
    x_p = list !! (p + 1)
    y_p = list !! (p + 2)
    x = list !! x_p
    y = list !! y_p
    target = list !! (p + 3)

tryForOutput :: (Int, Int) -> [Int] -> Maybe (Int, Int)
tryForOutput (noun, verb) list =
  case run 0 $ replace (1, noun) $ replace (2, verb) list of
    Left result ->
      if head result == 19690720
        then Just (noun, verb)
        else Nothing
    Right err -> trace err Nothing
