import MyUtils
import Text.Read
import Data.List
import Data.Maybe
import Control.Arrow

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input17.txt"

type Cubes = [[[Bool]]]
type Pos = (Int,Int,Int)
type Update = (Bool,Pos)

parseInput :: [String] -> Cubes
parseInput s = [map2 (=='#') s]

printCubes :: Cubes -> IO()
printCubes cubes = cubes |> map3 (\c-> if c then '#' else '.') |> map2 (++"\n") |> map concat |> map (++"\n-----------\n") |> concat |> putStrLn

padCubes :: Cubes -> Cubes
padCubes cubes = cubes |> map2 (\row -> False:row++[False]) |> map (\plane-> (createNewRow plane):plane++[createNewRow plane]) |> \space -> (createNewPlane space):space++[createNewPlane space]

createNewRow :: [[Bool]] -> [Bool]
createNewRow plane = replicate (length (plane!!0)) False

createNewPlane :: Cubes -> [[Bool]]
createNewPlane cubes = replicate (length (cubes!!0))  (createNewRow (cubes!!0))

getNeighbours :: Pos -> [Pos]
getNeighbours (x,y,z) = map (\(x2,y2,z2) -> (x+x2, y+y2, z+z2)) directions3D

countNeighbours :: Cubes -> Pos -> Int
countNeighbours c p = getNeighbours p |> count (getValue c)

getValue :: Cubes -> Pos -> Bool
getValue c (x,y,z) = c!!?z |> maybe Nothing (!!?y) |> maybe Nothing (!!?x) |> fromMaybe False

allPositions :: Cubes -> [Pos]
allPositions c = indexes c |> map (\z-> indexes (c!!0) |> map (\y-> indexes (c!!0!!0) |> map (\x-> (x,y,z)))) |> concat |> concat

updateCube :: Cubes -> Pos -> Maybe Update
updateCube c p = case getValue c p of
    True  -> if neighbours == 2 || neighbours == 3 then Nothing else Just (False, p)
    False -> if neighbours == 3 then Just (True, p) else Nothing    
    where neighbours = countNeighbours c p

applyUpdate :: Cubes -> Update -> Cubes
applyUpdate c (v,(x,y,z)) = setElement3 x y z v c


updateStep :: Cubes -> Cubes
updateStep c = allPositions c' |> mapMaybe (updateCube c') |> foldl applyUpdate c' where c' = padCubes c

part1 :: [String] -> Int
part1 s = s |> parseInput |> repeatF 6 updateStep |> map2 (count id) |> map sum |> sum





input :: [String]
input = ["##..#.#.", "###.#.##", "..###..#", ".#....##", ".#..####", "#####...", "#######.", "#.##.#.#"]

--getValue' :: Cubes -> Pos -> Bool
--getValue' c (x,y,z) = c!!?z |> fmap (!!?y) |> flattenMaybe |> fmap (!!?x) |> flattenMaybe |> fromMaybe False

--getNeighbours :: Pos -> [Pos]
--getNeighbours (x,y,z) = getNeighbours' 1 (x,y,z)
--
--getNeighbours' :: Int -> Pos -> [Pos]
--getNeighbours' 1 (x,y,z) = [(x-1,y,z), (x,y,z), (x+1,y,z)] |> map (getNeighbours' 2) |> concat
--getNeighbours' 2 (x,y,z) = [(x,y-1,z), (x,y,z), (x,y+1,z)] |> map (getNeighbours' 3) |> concat
--getNeighbours' 3 (x,y,z) = [(x,y,z-1), (x,y,z), (x,y,z+1)]
