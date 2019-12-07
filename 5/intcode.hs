import           Debug.Trace
import           System.IO
import           System.IO.Unsafe

main = do
  raw <- readFile "input.txt"
  runRaw raw

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

{-# NOINLINE unsafeReadLine #-}
unsafeReadLine = unsafePerformIO getLine

debug :: Show a => a -> a
debug v = trace (show v) v

run :: Int -> [Int] -> IO (Either [Int] String)
run p list =
  let instruction = instructionFrom p list
   in case debug instruction of
        Right err -> return $ Right err
        Left instr ->
          case instr of
            Add x y t -> run (p + 4) $ replace (t, x + y) list
            Mul x y t -> run (p + 4) $ replace (t, x * y) list
            SaveInp t -> do
              line <- getLine
              run (p + 2) $ replace (t, read line) list
            OutPut t -> run (p + 2) $ trace (show t) list
            Jt cmp t ->
              run
                (if cmp /= 0
                   then t
                   else p + 3)
                list
            Jf cmp t ->
              run
                (if cmp == 0
                   then t
                   else p + 3)
                list
            Less x y t ->
              let num =
                    if x < y
                      then 1
                      else 0
               in run (p + 4) $ replace (t, num) list
            Eq x y t ->
              let num =
                    if x == y
                      then 1
                      else 0
               in run (p + 4) $ replace (t, num) list
            Exit -> return $ Left list

runRaw :: String -> IO ()
runRaw raw = do
  result <- run 0 $ splitNums (== ',') raw
  case result of
    Left final_state -> putStrLn "success"
    Right err        -> putStrLn err

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
