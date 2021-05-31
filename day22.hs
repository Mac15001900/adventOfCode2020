import MyUtils
import Text.Read
import Data.List
import Data.Maybe
import Control.Arrow
import Data.Char

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input22.txt"

type Deck = [Int]

playGame :: Deck -> Deck -> Int
playGame xs [] = score (reverse xs) 1
playGame [] ys = score (reverse ys) 1
playGame (x:xs) (y:ys)  | x>y       = playGame (xs++[x,y]) ys
                        | otherwise = playGame xs (ys++[y,x])

score :: Deck -> Int -> Int
score [] _ = 0
score (x:xs) n = x*n + (score xs (n+1))

part1 :: [String] -> Int
part1 s = s |> splitOn "" |> map tail |> map (map read) |> \[a,b]-> playGame a b

--Part2--

type History = [(Deck,Deck)]

playedBefore :: History -> Deck -> Deck -> Bool
playedBefore h d1 d2 = exists (==(d1,d2)) h

canRecurse :: Int -> Deck -> Int -> Deck -> Bool
canRecurse x xs y ys = length xs >= x && length ys >= y

playGame2 :: History -> Deck -> Deck -> (Bool,Int) --(Winner, score)
playGame2 _ xs [] = (True , score (reverse xs) 1)
playGame2 _ [] ys = (False, score (reverse ys) 1)
playGame2 h (x:xs) (y:ys) | playedBefore h (x:xs) (y:ys) = (True, score (reverse (x:xs)) 1)
                          | canRecurse x xs y ys = if playGame2 [] (take x xs) (take y ys) |> fst then playGame2 (((x:xs),(y:ys)):h) (xs++[x,y]) ys else playGame2 (((x:xs),(y:ys)):h) xs (ys++[y,x])
                          | x>y       = playGame2 (((x:xs),(y:ys)):h) (xs++[x,y]) ys
                          | otherwise = playGame2 (((x:xs),(y:ys)):h) xs (ys++[y,x])

part2 :: [String] -> Int
part2 s = s |> splitOn "" |> map tail |> map (map read) |> \[a,b]-> playGame2 [] a b |> snd


