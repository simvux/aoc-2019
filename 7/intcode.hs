import           Data.List
import           Debug.Trace
import           System.Environment
import           System.IO
import           System.IO.Unsafe

main = do
  args <- getArgs
  raw <- readFile $ last args
  let numeric = splitNums (== ',') raw
   in print $ map (tryNumP1 numeric) $ permutations [5, 6, 7, 8, 9]
    {-
tryNumP2 :: [Int] -> (Int, [[Int]]) -> Int
tryNumP2 list (i, all_comb) =
  (\(list, n2) -> snd $ unwrap $run ([comb !! 4, n2], []) 0 list) $
  (\(list, n2) -> unwrap $run ([comb !! 3, n2], []) 0 list) $
  (\(list, n2) -> unwrap $run ([comb !! 2, n2], []) 0 list) $
  (\(list, n2) -> unwrap $run ([comb !! 1, n2], []) 0 list) $
  unwrap $ run ([head comb, 0], []) 0 list
  where
    comb = all_comb !! i
-}

tryNumP1 :: [Int] -> [Int] -> Int
tryNumP1 list comb =
  (\n2 -> unwrap $run ([comb !! 4, n2], []) 0 list) $
  (\n2 -> unwrap $run ([comb !! 3, n2], []) 0 list) $
  (\n2 -> unwrap $run ([comb !! 2, n2], []) 0 list) $
  (\n2 -> unwrap $run ([comb !! 1, n2], []) 0 list) $
  unwrap $ run ([head comb, 0], []) 0 list

unwrap :: Either a String -> a
unwrap v =
  case v of
    Left ok   -> ok
    Right err -> error err

splitNums :: (Char -> Bool) -> String -> [Int]
splitNums p s =
  case dropWhile p s of
    "" -> []
    s' -> read w : splitNums p s''
      where (w, s'') = break p s'

replace :: (Int, a) -> [a] -> [a]
replace _ [] = []
replace (0, a) (_:xs) = a : xs
replace (n, a) (x:xs) =
  if n < 0
    then x : xs
    else x : replace (n - 1, a) xs

(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
  | i < 0 = Nothing
  | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing

run :: ([Int], [Int]) -> Int -> [Int] -> Either Int String
run (input, output) p list =
  let instruction = instructionFrom p list
   in case instruction of
        Right err -> Right err
        Left instr ->
          case instr of
            Add x y t -> run (input, output) (p + 4) $ replace (t, x + y) list
            Mul x y t -> run (input, output) (p + 4) $ replace (t, x * y) list
            SaveInp t ->
              run (drop 1 input, output) (p + 2) $ replace (t, head input) list
            OutPut t -> run (t : input, t : output) (p + 2) list
            Jt cmp t ->
              run
                (input, output)
                (if cmp /= 0
                   then t
                   else p + 3)
                list
            Jf cmp t ->
              run
                (input, output)
                (if cmp == 0
                   then t
                   else p + 3)
                list
            Less x y t ->
              let num =
                    if x < y
                      then 1
                      else 0
               in run (input, output) (p + 4) $ replace (t, num) list
            Eq x y t ->
              let num =
                    if x == y
                      then 1
                      else 0
               in run (input, output) (p + 4) $ replace (t, num) list
            Exit -> Left $ head output

type Position = Int

data Instruction
  = Add Int Int Position
  | Mul Int Int Position
  | SaveInp Position
  | OutPut Position
  | Jt Int Int
  | Jf Int Int
  | Less Int Int Int
  | Eq Int Int Int
  | Exit
  deriving (Show)

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

instructionFrom :: Int -> [Int] -> Either Instruction String
instructionFrom pos list =
  case opcode of
    99 -> Left Exit
    1 -> Left $ Add (param 1) (param 2) (target 3)
    2 -> Left $ Mul (param 1) (param 2) (target 3)
    3 -> Left $ SaveInp (target 1)
    4 -> Left $ OutPut (param 1)
    5 -> Left $ Jt (param 1) (param 2)
    6 -> Left $ Jf (param 1) (param 2)
    7 -> Left $ Less (param 1) (param 2) (target 3)
    8 -> Left $ Eq (param 1) (param 2) (target 3)
    _ ->
      Right $
      "Invalid opcode: " ++
      show opcode ++ " at action " ++ action ++ " with position " ++ show pos
  where
    action = show $ list !! pos
    opcode = read $ lastN 2 action
    target n = list !! (pos + n)
    mode n =
      if length action < 3
        then 0
        else case reverse (take (length action - 2) action) !!? n of
               Just m  -> read [m]
               Nothing -> 0
    param n =
      let v = list !! (n + pos)
       in case mode (n - 1) of
            0 -> list !! v
            1 -> v
            m ->
              trace
                ("Warning: Invalid mode " ++
                 show m ++
                 " for action " ++ action ++ " positioned at " ++ show pos)
                v
