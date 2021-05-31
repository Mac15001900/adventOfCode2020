import MyUtils
import Text.Read
import Data.List
import Data.Maybe
import Control.Arrow
import Data.Char

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input24.txt"

type Pos = (Int,Int)

move :: Pos -> String -> Pos
move pos [] = pos
move (x,y) ('w':xs) = move (x-1,y) xs
move (x,y) ('e':xs) = move (x+1,y) xs
move (x,y) ('n':'w':xs) = move (x,y+1) xs
move (x,y) ('n':'e':xs) = move (x+1,y+1) xs
move (x,y) ('s':'w':xs) = move (x-1,y-1) xs
move (x,y) ('s':'e':xs) = move (x,y-1) xs

part1 :: [String] -> Int
part1 input = flips |> unique |> filter (\f->count (==f) flips |> (flip mod) 2 |> (==1)) |> length   where flips = map (move (0,0)) input

--Part 2--

type Tile = Bool -- False is white, True is black
type Floor = [[Tile]]

whites :: Int -> [Tile]
whites n = repeat False |> take n

whites2 :: Int -> Int -> Floor
whites2 x y = repeat (whites x) |> take y