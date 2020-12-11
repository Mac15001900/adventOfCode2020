import MyUtils
import Text.Read
import Data.List
import Data.Maybe

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input11.txt"

data Tile = Free | Taken | Floor deriving (Eq, Read) --Represents a single tile
type Room = [[Tile]] --Represents all tiles in the waiting room
type Pos = (Int,Int) --Represents a position in the room
data SeatData = Seat Pos [Pos] deriving (Eq, Read, Show) --Represents a seat and its neighbours

instance Show Tile where
    show Free = "L"
    show Taken = "#"
    show Floor = "."

--Input processing
readTile :: Char -> Tile
readTile 'L' = Free
readTile '#' = Taken
readTile '.' = Floor

readInput :: [String] -> Room
readInput [] = []
readInput (x:xs) = (map readTile x):(readInput xs)

--Room operators
getTile :: Room -> Pos -> Tile
getTile r (x,y) = r!!y!!x

setTile :: Room -> (Pos, Tile) -> Room
setTile r ((x,y),t) = take y r ++ [take x (r!!y) ++ [t] ++ drop (x+1) (r!!y)] ++ drop (y+1) r


--Neighbour finding - common
posNeighbours :: Pos -> [Pos]
posNeighbours (x,y) = [(x+1,y+1), (x+1,y), (x+1,y-1), (x,y+1), (x,y-1), (x-1,y+1), (x-1,y), (x-1,y-1)]

inBounds :: Room -> Pos -> Bool
inBounds r (x,y) = x>=0 && y>=0 && x<length (r!!0) && y<length r

allPositions :: Room -> [Pos]
allPositions r = [0..length (r!!0) - 1] |> map (\x-> [0..length r - 1] |> map (\y->(x,y))) |> concat

countTaken :: Room -> SeatData -> Int
countTaken r (Seat _ []) = 0
countTaken r (Seat p0 (p:ps)) = current + countTaken r (Seat p0 ps) where current = if getTile r p == Taken then 1 else 0

--Neighbour finding - part 1
validNeighbours :: Room -> Pos -> [Pos]
validNeighbours r p = posNeighbours p |> filter (inBounds r)

--Neighbour finding - part 2
seenFrom :: Room -> Pos -> [Pos]
seenFrom r p = posNeighbours (0,0) |> map (seenInDirection r p) |> removeNothing

seenInDirection :: Room -> Pos -> (Int,Int) -> Maybe Pos
seenInDirection r p s = seenInDirection' r (addComponents p s) s

seenInDirection' :: Room -> Pos -> (Int,Int) -> Maybe Pos
seenInDirection' r p s = if inBounds r p then if getTile r p == Floor then seenInDirection' r (addComponents p s) s else Just p else Nothing

addComponents :: (Int,Int) -> (Int,Int) -> (Int,Int)
addComponents (x1,y1) (x2,y2) = (x1+x2, y1+y2)


--The automata mechanics
updateSeat :: Int -> Room -> SeatData -> Maybe (Pos, Tile)
updateSeat t r (Seat p n) = 
    if tile==Floor then Nothing else 
    if tile==Taken && count >= t then Just (p,Free) else
    if tile==Free && count == 0 then Just (p,Taken) else
    Nothing where
        tile = getTile r p
        count = countTaken r (Seat p n)

applyUpdates :: Room -> [Maybe (Pos, Tile)] -> Room
applyUpdates r [] = r
applyUpdates r (Nothing:xs) = applyUpdates r xs
applyUpdates r ((Just u):xs) = applyUpdates (setTile r u) xs

updateUntilSame :: Int -> [SeatData] -> Room -> Room
updateUntilSame t s r = if r==r' then r else updateUntilSame t s r' where r' = s |> map (updateSeat t r) |> applyUpdates r


--Solutions
solution :: Int -> (Room->Pos->[Pos]) -> [String] -> Int
solution threshold neighbourFunction input = updateUntilSame threshold seats room |> concat |> count (==Taken) where
    room = readInput input
    seats = allPositions room |> map (\pos-> Seat pos (neighbourFunction room pos))

part1 :: [String] -> Int
part1 = solution 4 validNeighbours

part2 :: [String] -> Int
part2 = solution 5 seenFrom


--Debugging
nicePrint :: Room -> IO()
nicePrint r = r |> map show |> map (++"\n") |> concat |> putStrLn

test = ["L.LL.LL.LL", "LLLLLLL.LL", "L.L.L..L..", "LLLL.LL.LL", "L.LL.LL.LL", "L.LLLLL.LL", "..L.L.....", "LLLLLLLLLL", "L.LLLLLL.L", "L.LLLLL.LL"]


















