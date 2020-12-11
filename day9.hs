import MyUtils
import Text.Read
import Data.List
import Data.Maybe

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input9.txt"

valid :: [Int] -> Int -> Bool
valid xs target = xs |> count (\x-> exists (\y-> y==target-x && x/=y) xs) |>  (>0)

--            Preamble   Rest
findInvalid :: [Int] -> [Int] -> Int
findInvalid (p:ps) (r:rs) = if valid (p:ps) r then findInvalid (ps++[r]) rs else r
findInvalid _ [] = error "Everything seems valid"

part1 :: [String] -> Int
part1 input = findInvalid (take 25 numbers) (drop 25 numbers) where numbers = map read input

--Part 2--

findAnswer :: [Int] -> Int
findAnswer xs = (foldr1 min xs) + (foldr1 max xs)

--              Candidates Target Buffer
findComponents :: [Int] -> Int -> [Int] -> Maybe [Int]
findComponents [] _ _ = Nothing
findComponents (x:xs) t b = if summed==t then Just (x:b) else if summed>t then Nothing else findComponents xs t (x:b)
    where summed = x+(sum b)

checkAll :: Int -> [Int] -> Int
checkAll t (x:xs) = case findComponents (x:xs) t [] of
    Nothing -> checkAll t xs
    Just a -> findAnswer a
    
part2 :: [String] -> Int
part2 input = checkAll (part1 input) (map read input)

--Too low: 15095125