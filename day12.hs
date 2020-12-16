import MyUtils
import Text.Read
import Data.List
import Data.Maybe

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input12.txt"

type Pos = (Int,Int,Int) --X, Y, Rotation
type Instruction = (Char,Int)

fixAngle :: Int -> Int
fixAngle a = if a >=360 then fixAngle (a-360) else if a<0 then fixAngle (a+360) else a

parseInstruction :: String -> Instruction
parseInstruction (c:n) = (c,read n)

applyInstruction :: Pos -> Instruction -> Pos
applyInstruction (x,y,r) ('N', n) = (x, y+n, r)
applyInstruction (x,y,r) ('S', n) = (x, y-n, r)
applyInstruction (x,y,r) ('E', n) = (x+n, y, r)
applyInstruction (x,y,r) ('W', n) = (x-n, y, r)
applyInstruction (x,y,r) ('L', n) = (x, y, fixAngle (r+n))
applyInstruction (x,y,r) ('R', n) = (x, y, fixAngle (r-n))
applyInstruction pos     ('F', n) = forward pos n

forward :: Pos -> Int -> Pos
forward (x,y,0) n = (x+n, y, 0)
forward (x,y,90) n = (x, y+n, 90)
forward (x,y,180) n = (x-n, y, 180)
forward (x,y,270) n = (x, y-n, 270)

part1 :: [String] -> Int
part1 input = input |> map parseInstruction |> foldl applyInstruction (0,0,0) |> \(x,y,_)->(abs x)+(abs y)

test = ["F10","N3","F7","R90","F11"]

--Part 2--

type Ship = (Int,Int,Int,Int) -- Ship X, ship Y, waypoint X, waypoint Y

applyInstruction2 :: Ship -> Instruction -> Ship
applyInstruction2 ship          (_, 0)   = ship
applyInstruction2 (sx,sy,wx,wy) ('N',n) = (sx, sy, wx, wy+n)
applyInstruction2 (sx,sy,wx,wy) ('S',n) = (sx, sy, wx, wy-n)
applyInstruction2 (sx,sy,wx,wy) ('E',n) = (sx, sy, wx+n, wy)
applyInstruction2 (sx,sy,wx,wy) ('W',n) = (sx, sy, wx-n, wy)
applyInstruction2 (sx,sy,wx,wy) ('R',n) = applyInstruction2 (sx, sy, wy, -wx) ('R',n-90)
applyInstruction2 (sx,sy,wx,wy) ('L',n) = applyInstruction2 (sx, sy, -wy, wx) ('L',n-90)
applyInstruction2 (sx,sy,wx,wy) ('F',n) = (sx+wx*n, sy+wy*n, wx, wy)

part2 :: [String] -> Int
part2 input = input |> map parseInstruction |> foldl applyInstruction2 (0,0,10,1) |> \(x,y,_,_)->(abs x)+(abs y)