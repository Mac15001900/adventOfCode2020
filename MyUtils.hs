

module MyUtils (runOnFile,runOnFile2,(|>),split,count,freq,exists,unique,unique',rotateMatrix,splitOn,joinWith,valueBetween, differences, tupleMap, repeatF, removeNothing, indexes, zipWithIndexes, map2, setElement, empty2) where
import Control.Monad
import Data.List
import Data.Maybe
import System.IO

(|>) :: a -> (a->b) -> b
a |> f = f a

--Takes a file path and a function, runs that function on the file's contents, and prints the function's output. Trims the last line of the file if it's an empty line
runOnFile :: Show a => String -> ([String]->a) -> IO ()
runOnFile input start = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = splitOn '\n' contents
   let linesTrimmed = if last lines == "" then init lines else lines
   print $ start linesTrimmed
   hClose handle
   
runOnFile2 :: ([String]->String) -> String -> IO ()
runOnFile2 start input = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = split (=='\n') contents
   putStrLn $ start lines
   hClose handle
   
split     :: (a -> Bool) -> [a] -> [[a]]
split p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'

count :: (a->Bool) -> [a] -> Int
count p = length . (filter p)

freq :: Eq a => [a] -> a -> Int
freq [] _     = 0
freq (x:xs) a = (if x==a then 1 else 0) + (freq xs a)

exists :: (a->Bool) -> [a] -> Bool
exists p xs = isJust (find p xs) 


unique :: Eq a  => [a] -> [a]
unique xs = xs |> reverse |> unique' |> reverse

unique' :: Eq a => [a] -> [a]
unique' []     = []
unique' (x:xs) = if freq xs x >0 then unique' xs else x:unique' xs

rotateMatrix :: [[a]] -> [[a]]
rotateMatrix (x:xs) = foldr largerZip (map (\a->[a]) x) (reverse xs) |> map reverse

largerZip :: [a] -> [[a]] -> [[a]]
largerZip []     []       = []
largerZip (x:xs) (ys:yss) = (x:ys):(largerZip xs yss)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a xs = splitOn' a xs []

splitOn' :: Eq a => a -> [a] -> [a]-> [[a]]
splitOn' a [] op     = [reverse op]
splitOn' a (x:xs) op = if a==x then (reverse op):(splitOn' a xs []) else splitOn' a xs (x:op)

joinWith :: [a] -> [[a]] -> [a]
joinWith a [] = []
joinWith a [x] = x
joinWith a (x:xs) = (x++a)++(joinWith a xs)

valueBetween :: Ord a => (a,a) -> a -> Bool
valueBetween (low,high) x = x >= low && x <= high

differences :: Num a => [a] -> [a]
differences [] = []
differences a = zip (tail a) (init a) |>  tupleMap (-)

tupleMap :: (a->b->c) -> [(a,b)] -> [c]
tupleMap f = map (\(a,b) -> f a b)

repeatF :: (a->a) -> Int -> a -> a
repeatF _ 0 x = x
repeatF f n x = repeatF f (n-1) (f x)

removeNothing :: [Maybe a] -> [a]
removeNothing [] = []
removeNothing (Nothing:xs) = removeNothing xs
removeNothing ((Just a):xs) = a:(removeNothing xs)

indexes :: [a] -> [Int]
indexes [] = []
indexes a = [0..(length a)-1]

zipWithIndexes :: [a] -> [(a,Int)]
zipWithIndexes a = zip a (indexes a)

map2 :: (a->b) -> [[a]] -> [[b]]
map2 f = map (map f)

empty2 :: Eq a => [[a]] -> Bool
empty2 xs = not $ exists (/=[]) xs

setElement :: Int -> a -> [a] -> [a]
setElement i x xs = (take i xs)++[x]++(drop (i+1) xs)

