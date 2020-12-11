import MyUtils
import Text.Read
import Data.List
import Data.Maybe

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input10.txt"

diff :: [String] -> [Int]
diff input = zip (sorted++[(last sorted)+3]) (0:sorted) |> map (\(a,b)->a-b) where sorted = sort (map read input)

part1 :: [String] -> Int
part1 input = diff input |> (\d-> (count (==1) d) * (count (==3) d))

--Part 2--
countPossibilities :: Int -> Int
countPossibilities 0 = 1
countPossibilities 1 = 1
countPossibilities 2 = 2
countPossibilities 3 = 4
countPossibilities 4 = 7

part2 :: [String] -> Int
part2 input = input |> diff |> splitOn 3 |> map length |> map countPossibilities |> foldr1 (*)