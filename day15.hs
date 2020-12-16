import MyUtils
import Text.Read
import Data.List
import Data.Maybe
import Control.Arrow

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input15.txt"

lastSpoken :: Int -> [Int] -> Int
lastSpoken = lastSpoken' 1

lastSpoken' :: Int -> Int -> [Int] -> Int
lastSpoken' _ _ [] = 0
lastSpoken' b n (x:xs) = if n==x then b else lastSpoken' (b+1) n xs

lastSpoken2 :: Int -> [(Int,Int)] -> Int
lastSpoken2 x xs  = case find (first (==x)) of
    Nothing    -> 0
    Just (a,b) -> b

addNewNumber :: [Int] -> [Int]
addNewNumber (x:xs) = (lastSpoken x xs):x:xs

addNewNumber2 :: [(Int,Int)] -> [(Int,Int)]
addNewNumber2 (x:xs) = ((lastSpoken2 x xs), ):x:xs

input :: [Int]
input = [6,13,1,15,2,0]

part1 :: [Int] -> Int -> Int
part1 start target = start |> reverse |> repeatF addNewNumber (target - (length start)) |> head
