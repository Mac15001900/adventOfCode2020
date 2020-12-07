import MyUtils
import Text.Read

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input5.txt"

--F=0, B=1, L=0, R=1

--Pass the reversed string to it
decodeSeatId :: String -> Int
decodeSeatId [] = 0
decodeSeatId (x:xs) = extra + 2*(decodeSeatId xs) where extra = if elem x "FL" then 0 else 1

part1 :: [String] -> Int
part1 input = input |> map reverse |> map decodeSeatId |> foldr max 0

part2 :: [String] -> Int
part2 input = [0..1023] |> filter (not . ((flip elem) ids)) |> filter (isSurrounded ids) |> (flip (!!)) 0 where
    ids = input |> map reverse |> map decodeSeatId
    
isSurrounded :: [Int] -> Int -> Bool
isSurrounded xs x = (elem (x-1) xs) && (elem (x+1) xs)

