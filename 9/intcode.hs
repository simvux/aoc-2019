import           Debug.Trace
import           System.IO
import           System.IO.Unsafe

main = do
  raw <- readFile "input.txt"
  runRaw raw [2]

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

debug :: Show a => a -> a
debug v = trace (show v) v

data Mem =
  Mem
    { input  :: [Int]
    , output :: [Int]
    , src    :: [Int]
    , r_ip   :: Int
    , r_base :: Int
    }
  deriving (Show)

run :: Mem -> Either Mem String
run mem =
  let instruction = instructionFrom (r_ip mem) (r_base mem) (src mem)
   in case instruction of
        Right err -> Right err
        Left instr ->
          case instr of
            Add x y t ->
              run $
              mem {r_ip = r_ip mem + 4, src = replace (t, x + y) $ src mem}
            Mul x y t ->
              run $
              mem {r_ip = r_ip mem + 4, src = replace (t, x * y) $ src mem}
            SaveInp t ->
              run $
              mem
                { r_ip = r_ip mem + 2
                , src = replace (t, head $ input mem) $ src mem
                , input = drop 1 (input mem)
                }
            OutPut t ->
              run $ mem {r_ip = r_ip mem + 2, output = output mem ++ [t]}
            Jt cmp t ->
              run $
              mem
                { r_ip =
                    if cmp /= 0
                      then t
                      else r_ip mem + 3
                }
            Jf cmp t ->
              run $
              mem
                { r_ip =
                    if cmp == 0
                      then t
                      else r_ip mem + 3
                }
            Less x y t ->
              let num =
                    if x < y
                      then 1
                      else 0
               in run $
                  mem {r_ip = r_ip mem + 4, src = replace (t, num) $ src mem}
            Eq x y t ->
              let num =
                    if x == y
                      then 1
                      else 0
               in run $
                  mem {r_ip = r_ip mem + 4, src = replace (t, num) $ src mem}
            SetBase v ->
              run $ mem {r_ip = r_ip mem + 2, r_base = v + r_base mem}
            Exit -> Left mem

runRaw :: String -> [Int] -> IO ()
runRaw raw inp =
  let result =
        run $
        Mem
          { r_ip = 0
          , src = splitNums (== ',') raw ++ replicate 500 0
          , input = inp
          , output = []
          , r_base = 0
          }
   in case result of
        Left final_state -> print $ output final_state
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
  | SetBase Int
  | Exit
  deriving (Show)

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

instructionFrom :: Int -> Int -> [Int] -> Either Instruction String
instructionFrom pos rbase list =
  case opcode of
    99 -> Left Exit
    1 -> Left $ Add (param 1) (param 2) (param_write 3)
    2 -> Left $ Mul (param 1) (param 2) (param_write 3)
    3 -> Left $ SaveInp (param_write 1)
    4 -> Left $ OutPut (param 1)
    5 -> Left $ Jt (param 1) (param 2)
    6 -> Left $ Jf (param 1) (param 2)
    7 -> Left $ Less (param 1) (param 2) (param_write 3)
    8 -> Left $ Eq (param 1) (param 2) (param_write 3)
    9 -> Left $ SetBase (param 1)
    _ ->
      Right $
      "Invalid opcode: " ++
      show opcode ++ " at action " ++ action ++ " with position " ++ show pos
  where
    action = show $ list !! pos
    opcode = read $ lastN 2 action
    mode n =
      if length action < 3
        then 0
        else case reverse (take (length action - 2) action) !!? n of
               Just m  -> read [m]
               Nothing -> 0
    param_write n =
      let v = list !! (n + pos)
       in case mode (n - 1) of
            0 -> v
            1 ->
              error $
              "immediate mode on write " ++ show opcode ++ " at " ++ show pos
            2 -> v + rbase
            m ->
              trace
                ("Warning: Invalid mode " ++
                 show m ++
                 " for action " ++ action ++ " positioned at " ++ show pos)
                v
    param n =
      let v = list !! (n + pos)
       in case mode (n - 1) of
            0 -> list !! v
            1 -> v
            2 -> list !! (v + rbase)
            m ->
              trace
                ("Warning: Invalid mode " ++
                 show m ++
                 " for action " ++ action ++ " positioned at " ++ show pos)
                v
