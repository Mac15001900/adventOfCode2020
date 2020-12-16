import MyUtils
import Text.Read
import Data.List
import Data.Maybe
import Control.Arrow

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input13.txt"

part1 :: [String] -> Int
part1 input = input!!1 |> splitOn ',' |> filter (/="x") |> map read |> foldr1 (\a b-> if timeTo a < timeTo b then a else b)  |> \a->(timeTo a)*a   where
    timeTo = timeToBus (input!!0 |> read)

--Current time -> bus period -> res
timeToBus :: Int -> Int -> Int
timeToBus c t = t - (c `mod` t)


--Part2--

type Solution = (Int,Int) --First occurence, next occurence
type Constraint = (Int,Int) -- (X,Y) means the result of mod X has to be (X-Y)

fixCostraint :: Constraint -> Constraint
fixCostraint (x,y) = if y>x then fixCostraint (x,y-x) else if y==0 then (x,x) else (x,y)

nextSolution :: Solution -> Constraint -> Solution
nextSolution (s,ds) (x,y) = if s `mod` x == (x-y) then (s,ds*x) else nextSolution (s+ds,ds) (x,y)

part2 :: [String] -> Int
part2 input = input!!1 |> splitOn ',' |> zipWithIndexes |> filter (\(a,b)->a/="x") |> map (first read) |> map fixCostraint |> foldl nextSolution (1,1) |> fst 

--map (\(a,b) -> (read a,b))

--filter (\(a,b)->a/="x")
--filter (fst . first (/="x"))