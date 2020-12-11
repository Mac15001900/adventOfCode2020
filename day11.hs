import MyUtils
import Text.Read
import Data.List
import Data.Maybe

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input11.txt"

data Tile = Free | Taken | Floor deriving (Eq, Read)
type Room = [[Tile]]
type Pos = (Int,Int)

instance Show Tile where
    show Free = "L"
    show Taken = "#"
    show Floor = "."

getTile :: Room -> Pos -> Tile
getTile r (x,y) = r!!y!!x

setTile :: Room -> (Pos, Tile) -> Room
setTile r ((x,y),t) = take y r ++ [take x (r!!y) ++ [t] ++ drop (x+1) (r!!y)] ++ drop (y+1) r

countTakenNeighbours :: Room -> Pos -> Int
countTakenNeighbours r (x,y) = posNeighbours (x,y) |> map (getTile r) |> map takenToInt |> sum

posNeighbours :: Pos -> [Pos]
posNeighbours (x,y) = [(x+1,y+1), (x+1,y), (x+1,y-1), (x,y+1), (x,y-1), (x-1,y+1), (x-1,y), (x-1,y-1)]

takenToInt :: Tile -> Int
takenToInt Taken = 1
takenToInt _ = 0

readInputWithPads :: [String] -> Room
readInputWithPads [] = []
readInputWithPads (x:xs) = (map readTile ('.':x++".")):(readInputWithPads xs)

readTile :: Char -> Tile
readTile 'L' = Free
readTile '#' = Taken
readTile '.' = Floor

padWithFloor :: Room -> Room
padWithFloor r = (floor:r)++[floor] where floor = (take (length (r!!0))) (repeat Floor)


updateTile :: Room -> Pos -> Tile
updateTile r p  = updateTile' r p (getTile r p)

updateTile' :: Room -> Pos -> Tile -> Tile
updateTile' r p Free = if countTakenNeighbours r p == 0 then Taken else Free
updateTile' r p Taken = if countTakenNeighbours r p >= 4 then Free else Taken
updateTile' _ _ Floor = Floor

roomsEqual :: Room -> Room -> Bool
roomsEqual [] [] = True
roomsEqual (x:xs) (y:ys) = if tupleMap (==) (zip x y) |> and then roomsEqual xs ys else False
roomsEqual [] _ = False
roomsEqual _ [] = False

updateRoom :: Room -> Room
updateRoom r = map (\x-> map (\y-> ((x,y), updateTile r (x,y))) [0..ly] ) [0..lx] |> concat |> foldl setTile r where  
    lx = length (r!!0) -1
    ly = length r -1

updateUntilSame :: Room -> Room
updateUntilSame r = if r'==r then r else updateUntilSame r' where r' = updateRoom r

countOccupied :: Room -> Int
countOccupied r = r |> concat |> count (==Taken)

part1 :: [String] -> Int
part1 input = input |> readInputWithPads |> padWithFloor |> updateUntilSame |> countOccupied

nicePrint :: Room -> IO()
nicePrint r = r |> map show |> map (++"\n") |> concat |> putStrLn

test = ["L.LL.LL.LL", "LLLLLLL.LL", "L.L.L..L..", "LLLL.LL.LL", "L.LL.LL.LL", "L.LLLLL.LL", "..L.L.....", "LLLLLLLLLL", "L.LLLLLL.L", "L.LLLLL.LL"] -- |> readInput

--Part2--

readInput :: [String] -> Room
readInput [] = []
readInput (x:xs) = (map readTile x):(readInput xs)

--Seat data stores position, state, and seen positions
data SeatData = Seat Pos [Pos] deriving (Eq, Read, Show)
type RoomData = (Room,[SeatData])

createRoomData :: Room -> RoomData
createRoomData r = (r, allPositions r |> map (createSeatData r))

createSeatData :: Room -> Pos -> SeatData
createSeatData r pos = Seat pos (seenFrom r pos)

seenFrom :: Room -> Pos -> [Pos]
seenFrom r p = posNeighbours (0,0) |> map (seenInDirection r p) |> removeNothing

seenInDirection :: Room -> Pos -> (Int,Int) -> Maybe Pos
seenInDirection r p s = seenInDirection' r (addComponents p s) s

seenInDirection' :: Room -> Pos -> (Int,Int) -> Maybe Pos
seenInDirection' r p s = if inBounds r p then if getTile r p == Floor then seenInDirection' r (addComponents p s) s else Just p else Nothing

addComponents :: (Int,Int) -> (Int,Int) -> (Int,Int)
addComponents (x1,y1) (x2,y2) = (x1+x2, y1+y2)

inBounds :: Room -> Pos -> Bool
inBounds r (x,y) = x>=0 && y>=0 && x<length (r!!0) && y<length r

allPositions :: Room -> [Pos]
allPositions r = [0..length (r!!0) - 1] |> map (\x-> [0..length r - 1] |> map (\y->(x,y))) |> concat

countSeenTaken :: Room -> SeatData -> Int
countSeenTaken r (Seat _ []) = 0
countSeenTaken r (Seat p0 (p:ps)) = current + countSeenTaken r (Seat p0 ps) where current = if getTile r p == Taken then 1 else 0

updateSeat :: Room -> SeatData -> Maybe (Pos, Tile)
updateSeat r (Seat p n) = 
    if tile==Floor then Nothing else 
    if tile==Taken && count >= 5 then Just (p,Free) else
    if tile==Free && count == 0 then Just (p,Taken) else
    Nothing where
        tile = getTile r p
        count = countSeenTaken r (Seat p n)

applyUpdates :: Room -> [Maybe (Pos, Tile)] -> Room
applyUpdates r [] = r
applyUpdates r (Nothing:xs) = applyUpdates r xs
applyUpdates r ((Just u):xs) = applyUpdates (setTile r u) xs

updateUntilSame2 :: [SeatData] -> Room -> Room
updateUntilSame2 s r = if r==r' then r else updateUntilSame2 s r' where r' = s |> map (updateSeat r) |> applyUpdates r

part2 :: [String] -> Int
part2 input = updateUntilSame2 seats room |> concat |> count (==Taken) where
    room = readInput input
    seats = allPositions room |> map (createSeatData room)






























