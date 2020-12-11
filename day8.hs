import MyUtils
import Text.Read
import Data.Maybe

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input8.txt"

data Instruction = NOP Int | ACC Int | JMP Int deriving (Show, Eq, Read) 

--         The program      Line   Acc    New Line,Acc
execute :: [Instruction] -> Int -> Int -> (Int,Int)
execute p l a = case p!!l of
    NOP _ -> (l+1, a)
    ACC n -> (l+1, a+n)
    JMP n -> (l+n, a)

--            The program      Line   Acc   Visited  Result    
executeAll :: [Instruction] -> Int -> Int -> [Int] -> Int
executeAll p l a v = if elem l v then a else executeAll p l' a' (l:v) 
    where (l',a') = execute p l a
    
part1 :: [String] -> Int
part1 input = executeAll (map read input) 0 0 []

--Part2--

executeAll' :: [Instruction] -> Int -> Int -> [Int] -> Maybe Int
executeAll' p l a v = if elem l v then Nothing else if l == length p then Just a else executeAll' p l' a' (l:v) 
    where (l',a') = execute p l a
    
swap :: Instruction -> Instruction
swap (NOP n) = JMP n
swap (ACC n) = ACC n
swap (JMP n) = NOP n

swapN :: [Instruction] -> Int -> [Instruction]
swapN p n = (take n p) ++ [swap (p!!n)] ++ (drop (n+1) p)

part2 :: [String] -> [Maybe Int]
part2 input = [0..(length input)-1] |> map (swapN (map read input)) |> map (\p->executeAll' p 0 0 []) |> filter isJust 