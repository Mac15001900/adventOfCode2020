import MyUtils
import Text.Read

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input7.txt"

data BagData = BagData [(String, Int)] deriving (Show, Eq, Read)
data BagBank = BagBank [(String, BagData)] deriving (Show, Eq, Read)

bagContains :: BagBank -> String -> BagData -> Bool
bagContains b target (BagData []) = False
bagContains b target (BagData ((x,_):xs)) = if (x == target) || (bagContains b target (getBagData b x)) then True else  
    bagContains b target (BagData xs)

getBagData :: BagBank -> String -> BagData
getBagData (BagBank [])  _ = error "Bag not found"
getBagData (BagBank ((name, bData):bs)) target = if name == target then bData else getBagData (BagBank bs) target

processInput :: [String] -> BagBank
processInput input = BagBank (processInput' input)

processInput' :: [String] -> [(String, BagData)]
processInput' [] = []
processInput' (x:xs) = ((take 2 words |> concat), BagData (processData (drop 4 words))) : (processInput' xs)
    where words = splitOn ' ' x

processData :: [String] -> [(String, Int)]
processData [] = []
processData ("no":_) = [] --These "contain no other bags"
processData (n:w1:w2:xs) = ((w1++w2), read n) : (processData (drop 1 xs))

listOfData :: BagBank -> [BagData]
listOfData (BagBank b) = map snd b

part1 :: [String] -> Int
part1 input = listOfData bank |> count (bagContains bank "shinygold")
    where bank = processInput input
    
--Part 2--

countBagsInside :: BagBank -> BagData -> Int
countBagsInside b (BagData []) = 0
countBagsInside b (BagData ((name,amount):xs)) = amount + amount * (countBagsInside b (getBagData b name)) + (countBagsInside b (BagData xs))

part2 :: [String] -> Int
part2 input = countBagsInside bank (getBagData bank "shinygold") where bank = processInput input 