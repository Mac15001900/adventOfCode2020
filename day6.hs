import MyUtils
import Text.Read

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input6.txt"

part1 :: [String] -> Int
part1 input = input |> splitOn "" |> map concat |> map unique |> map length |> sum

combine :: String -> String -> String
combine a b = filter (\x-> elem x b) a

part2 :: [String] -> Int
part2 input = input |> splitOn "" |> map (foldr1 combine) |> map length |> sum

part2' :: [String] -> Int
part2' input = input |> splitOn "" |> map (foldr1 (\xs-> filter (flip elem xs))) |> map length |> sum
