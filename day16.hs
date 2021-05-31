import MyUtils
import Text.Read
import Data.List
import Data.Maybe
import Control.Arrow

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input16.txt"

type Rule = (String,Int,Int,Int,Int)
type Ticket = [Int]
type Solutions = [[Rule]] --For each field, a list of possible rules

ruleName :: Rule -> String
ruleName (name,_,_,_,_) = name

fitsRule :: Int -> Rule -> Bool
fitsRule x (_,a,b,c,d) = (x >= a && x <= b) || (x >= c && x <= d)

fitsAnyRule :: [Rule] -> Int -> Bool
fitsAnyRule r x = exists (fitsRule x) r

validTicket :: [Rule] -> Ticket -> Bool
validTicket r t = map (fitsAnyRule r) t |> exists not |> not

parseRule :: String -> Rule
parseRule s = (splitOn ':' s |> (!!0), read a,read b,read c, read d) where
    [a,b] = splitOn ':' s |> (!!1) |> splitOn ' ' |> (!!1) |> splitOn '-'
    [c,d] = splitOn ':' s |> (!!1) |> splitOn ' ' |> (!!3) |> splitOn '-'
    
parseTicket :: String -> Ticket
parseTicket s = splitOn ',' s |> map read

part1 :: [String] -> Int
part1 input = input |> dropWhile (not . (isPrefixOf "nearby tickets")) |> tail |> map parseTicket |> concat |> filter (not . (fitsAnyRule rules)) |> sum where
    rules = input |> splitOn "" |> (!!0) |> map parseRule

brokenRules :: [Rule] -> Int -> [Rule]
brokenRules r x = filter (not . (fitsRule x)) r

combineRules :: [Rule] -> [Rule] -> [Rule]
combineRules r1 r2 = unique' (r1++r2)

possibleRules :: [Rule] -> [Rule] -> [Rule]
possibleRules all broken = filter (not . ((flip elem) broken)) all

swapRC :: [[a]] -> [[a]]
swapRC a = indexes (a!!0) |> map (\x-> map (!!x) a)

emptySolution :: Int -> [Rule]
emptySolution len = repeat ("Failed",0,0,0,0) |> take len

solve :: Int -> Solutions -> [Rule]
solve l s = solve' (emptySolution l) s

solve' :: [Rule] -> Solutions -> [Rule]
solve' res left  = case find (\(r,i)->length r == 1) (zipWithIndexes left) of
    Nothing -> res
    Just ([r],i) -> solve' (setElement i r res) (map (removeRule r) left)
    Just _ -> error "In solve'"

removeRule :: Rule -> [Rule] -> [Rule]
removeRule r rs = filter (/=r) rs


part2 :: [String] -> Int
part2 input = input |> dropWhile (not . (isPrefixOf "nearby tickets")) |> tail |> map parseTicket |>  filter (validTicket rules) |> swapRC |> map2 (brokenRules rules) |> map (foldr1 combineRules) |> map (possibleRules rules) |> solve (length rules) |> map ruleName |> zipWithIndexes |> filter ((isPrefixOf "departure") . fst) |> map (\(n,i)-> find ((==i) . snd) myTicket) |> removeNothing |> map fst |> foldr1 (*) where
    rules = input |> splitOn "" |> (!!0) |> map parseRule
    myTicket = input |> splitOn "" |> (!!1) |> (!!1) |> parseTicket |> zipWithIndexes
