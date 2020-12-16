import MyUtils
import Text.Read
import Data.List
import Data.Maybe
import Control.Arrow
import Data.Bits

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input14.txt"

data MaskBit = One | Zero | X deriving (Show,Eq)
type Mask = [MaskBit]
type MaskPair = (Int,Int) --(AndMask, OrMask)
type Instruction = (Int,Int)
--type MaskedInstruction = (Instruction,Mask)
type Memory = [(Int,Int)] --Adress, value

parseInstruction :: String -> Instruction
parseInstruction s = (read address, read value) where
    address = s |> splitOn ']' |> (!!0) |> splitOn '[' |> (!!1)
    value = s |> splitOn '=' |> (!!1)
 

parseMask :: String -> MaskPair
parseMask s = (andMask 0 parsed, orMask 0 parsed) where parsed = parseMask' s

parseMask' :: String -> Mask
parseMask' [] = []
parseMask' ('1':xs) = One:(parseMask' xs)
parseMask' ('0':xs) = Zero:(parseMask' xs)
parseMask' ('X':xs) = X:(parseMask' xs)
parseMask' (_:xs) = parseMask' xs

andMask :: Int -> Mask -> Int
andMask buffer [] = buffer
andMask buffer (m:ms) = andMask (buffer*2+extra) ms where extra = if m==Zero then 0 else 1

orMask ::  Int -> Mask -> Int
orMask buffer []  = buffer
orMask buffer (m:ms) = orMask (buffer*2+extra) ms where extra = if m==One then 1 else 0

applyMask :: MaskPair -> Int -> Int
applyMask (andMask,orMask) x = x |> (.&.andMask) |> (.|.orMask)

writeToMemory :: Instruction -> Memory -> Memory
writeToMemory (a,v) [] = [(a,v)]
writeToMemory (a,v) ((a2,v2):ms) = if a==a2 then (a,v):ms else (a2,v2):(writeToMemory (a,v) ms)

part1:: [String] -> Int
part1 input = zip masks instructions |> map (\(m,i) -> map (second (applyMask m)) i) |> concat |> reverse |> foldr writeToMemory [] |> map snd |> sum     where
    masks = input |> filter (isPrefixOf "mask") |> map parseMask
    instructions = input |> split (isPrefixOf "mask") |> map (map parseInstruction)


testOld = ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X","mem[8] = 11","mem[7] = 101","mem[8] = 0"]

test = ["mask = 000000000000000000000000000000X1001X","mem[42] = 100","mask = 00000000000000000000000000000000X0XX","mem[26] = 1"]
--Part 2

floatMasks :: Mask -> [Mask]
floatMasks = floatMasks' []

floatMasks' :: Mask -> Mask -> [Mask]
floatMasks' b [] = [b]
floatMasks' b (One:ms) = floatMasks' (b++[One]) ms
floatMasks' b (Zero:ms) = floatMasks' (b++[X]) ms
floatMasks' b (X:ms) = (floatMasks' (b++[One]) ms)++(floatMasks' (b++[Zero]) ms)

parseMask2 :: String -> [MaskPair]
parseMask2 s = s|> parseMask' |> floatMasks |> map (\m->(andMask 0 m,orMask 0 m))  

applyMask2 :: [MaskPair] -> Instruction -> [Instruction]
applyMask2 ms (a,v) = ms |> map ((flip applyMask) a) |> map (\a2->(a2,v))

part2:: [String] -> Int
part2 input = zip masks instructions |> map (\(m,i) -> map (applyMask2 m) i)|> concat |> concat |> sortOn fst |> sumInstructions 0 where -- |> reverse |> foldr writeToMemory [] |> map snd |> sum     where
    masks = input |> filter (isPrefixOf "mask") |> map parseMask2
    instructions = input |> split (isPrefixOf "mask") |> map (map parseInstruction)

sumInstructions :: Int -> [Instruction] -> Int
sumInstructions b [] = b
sumInstructions b [i] = b+snd i
sumInstructions b (i1:i2:is) = if fst i1 == fst i2 then sumInstructions b (i2:is) else sumInstructions (b+snd i1) (i2:is)


--This one does work, but it's really inefficient
part2Old:: [String] -> Int
part2Old input = zip masks instructions |> map (\(m,i) -> map (applyMask2 m) i)|> concat |> concat |>  reverse |> foldr writeToMemory [] |> map snd |> sum where -- |> reverse |> foldr writeToMemory [] |> map snd |> sum     where
    masks = input |> filter (isPrefixOf "mask") |> map parseMask2
    instructions = input |> split (isPrefixOf "mask") |> map (map parseInstruction)









--'Manual' way

--parseMask :: String -> Mask
--parseMask s = parseMask s |> \m-> zip m (reverse [0..(length m)-1])

--applyMask :: Mask -> Int -> Int
--applyMask m n = dropZeros m n |> 
--
--dropZeros :: Mask -> Int -> Mask
--dropZeros [] x = []
--dropZeros ((Zero,_):m) x = dropZeros m x
--dropZeros ((X,n):m) x = if bitAt x n == 0 then dropZeros m x else ((X,n):m)
--
       ----Which bit|Number|Bit (0 or 1)
--bitAt :: Int -> Int -> Int
--bitAt x k = if (x `mod` (2^k)) == (x `mod` (2^(k+1))) then 0 else 1
--
--maskedBitAt :: Mask -> Int -> Int
--maskedBitAt m x k = case m!!k of
    --One -> 1
    --Zero -> 0
    --X -> getBitAt x k


