import MyUtils
import Parsing
import Text.Read
import Data.List
import Data.Maybe
import Control.Arrow
import Data.Char

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input19.txt"

run2 :: Show a => ([String] -> a) -> IO ()
run2 = runOnFile "input19B.txt"

run3 :: Show a => ([String] -> a) -> IO ()
run3 = runOnFile "input19C.txt"

type ParserList = [(Int,Parser String)]

--createParser :: [String] -> Int -> ParserList -> ParserList
--createParser s n l  | splitOn ' ' (s!!n) |> (!!1) |> (!!0) |> (=='"')  =  (n,symbol (splitOn '"' (s!!n) |> (!!1))):l 
--                    | otherwise = 

buildParser :: [String] -> Int -> Parser String
buildParser s n | splitOn ' ' (s!!n) |> (!!1) |> (!!0) |> (=='"')  =  symbol (splitOn '"' (s!!n) |> (!!1)) 
                | otherwise = (s!!n) |> splitOn ':' |> (!!1) |> (++" ") |> splitOn '|' |> map (splitOn ' ') |> map tail |> map init |> map2 readInt |> map2 (buildParser s) |> map (foldr1 combineParsers) |> foldr1 (<|>) 

readInt :: String -> Int
readInt s = case readMaybe s of
    Just n  -> n
    Nothing -> error ("Failed to parse |"++s++"|")


--if all isDigit s then read s else error ("Failed to parse "++s)

sortRules :: [String] -> [String]
sortRules rs = sortOn (\r-> (splitOn ':' r)!!0 |> readInt) rs


part1 :: [String] -> Int
part1 input = input |> splitOn "" |> (!!1) |> count (validParse p) 
    where p = input |> splitOn "" |> (!!0) |> sortRules |> \rs -> buildParser rs 0


combineParsers :: Parser String -> Parser String -> Parser String
combineParsers p1 p2 = do s1 <- p1
                          s2 <- p2
                          --return (s1++s2)
                          return ("("++s1++","++s2++")")

validParse :: Parser a -> String -> Bool
validParse p xs = case (parse p xs) of
                [(n,[])]  -> True
                [(_,out)] -> False
                []        -> False

--Part 2
parser8 :: Parser String -> Parser String
parser8 other = do s1 <- other
                   do s2 <- (parser8 other)
                      return (s1++s2)
                      <|> return s1
                      
parser9 :: Parser String -> Parser String
parser9 other = (symbol "SPARTA") <|> other
                   
parser7 :: Parser String -> Parser String
parser7 other = do s1 <- other
                   return s1
           
                    

parser11 :: Parser String -> Parser String -> Parser String
parser11 p1 p2 = do s1 <- p1
                    do s2 <- p2
                       return (s1++s2)
                       <|>
                       do s2 <- (parser11 p1 p2)
                          s3 <- p2
                          return (s1++s2++s3)


--8: 42 | 42 8
--11: 42 31 | 42 11 31

p42 = 4 ::Int
--p42 = 4 ::Int
p31 = 31 ::Int

buildParser2 :: [String] -> Int -> Parser String
buildParser2 s n | splitOn ' ' (s!!n) |> (!!1) |> (!!0) |> (=='"')  =  symbol (splitOn '"' (s!!n) |> (!!1)) 
                 | n == 8  = parser9 (buildParser2 s p42)
                 -- | n == 8  = buildParser2 s 42
                 -- | n == 11 = parser11 (buildParser2 s 42) (buildParser2 s 31)
                 | n == 11 = combineParsers (buildParser2 s 42) (buildParser2 s 31)
                 | otherwise = (s!!n) |> splitOn ':' |> (!!1) |> (++" ") |> splitOn '|' |> map (splitOn ' ') |> map tail |> map init |> map2 readInt |> map2 (buildParser2 s) |> map (foldr1 combineParsers) |> foldr1 (<|>) 

part2 :: [String] -> Int
part2 input = input |> splitOn "" |> (!!1) |> count (validParse p) 
    where p = input |> splitOn "" |> (!!0) |> sortRules |> \rs -> buildParser2 rs 0

test ::  [String] -> [String]
test input = input |> splitOn "" |> (!!1) |> filter (validParse p) 
    where p = input |> splitOn "" |> (!!0) |> sortRules |> \rs -> buildParser rs 0

test' ::  [String] -> [[(String,String)]]
test' input = input |> splitOn "" |> (!!1) |> map (parse p) 
    where p = input |> splitOn "" |> (!!0) |> sortRules |> \rs -> buildParser rs 0
    
test2 ::  [String] -> [String]
test2 input = input |> splitOn "" |> (!!1) |> filter (validParse p) 
    where p = input |> splitOn "" |> (!!0) |> sortRules |> \rs -> buildParser2 rs 0


--Testing
s = ["0: 2 1 2 | 3 8","1: \"a\"", "2: \"b\"", "3: 1", "4: 1 2", "5: 1", "6: 1", "7: 1", "8: 42"]
n = 0 :: Int

--Length of 42: 20 51 | 39 120
--120: 41 39 | 58 20
--51: 39 91 | 20 117
--117: 20 126

--Too low: 270

example = ["42: 9 14 | 10 1", "9: 14 27 | 1 26", "10: 23 14 | 28 1", "1: \"a\"", "11: 42 31", "5: 1 14 | 15 1", "19: 14 1 | 14 14", "12: 24 14 | 19 1", "16: 15 1 | 14 14", "31: 14 17 | 1 13", "6: 14 14 | 1 14", "2: 1 24 | 14 4", "0: 8 11", "13: 14 3 | 1 12", "15: 1 | 14", "17: 14 2 | 1 7", "23: 25 1 | 22 14", "28: 16 1", "4: 1 1", "20: 14 14 | 1 15", "3: 5 14 | 16 1", "27: 1 6 | 14 18", "14: \"b\"", "21: 14 1 | 1 14", "25: 1 1 | 1 14", "22: 14 14", "8: 42", "26: 14 22 | 1 20", "18: 15 15", "7: 14 5 | 1 21", "24: 14 1", "29: 1","30: 1","32: 1","33: 1","34: 1","35: 1", "36: 1", "37: 1", "38: 1", "39: 1", "40: 1", "41: 1","", "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa", "bbabbbbaabaabba", "babbbbaabbbbbabbbbbbaabaaabaaa", "aaabbbbbbaaaabaababaabababbabaaabbababababaaa", "bbbbbbbaaaabbbbaaabbabaaa", "bbbababbbbaaaaaaaabbababaaababaabab", "ababaaaaaabaaab", "ababaaaaabbbaba", "baabbaaaabbaaaababbaababb", "abbbbabbbbaaaababbbbbbaaaababb", "aaaaabbaabaaaaababaa", "aaaabbaaaabbaaa", "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa", "babaaabbbaaabaababbaabababaaab", "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"]

example2 = ["42: 9 14 | 10 1", "9: 14 27 | 1 26", "10: 23 14 | 28 1", "1: \"a\"", "11: 42 31 | 42 11 31", "5: 1 14 | 15 1", "19: 14 1 | 14 14", "12: 24 14 | 19 1", "16: 15 1 | 14 14", "31: 14 17 | 1 13", "6: 14 14 | 1 14", "2: 1 24 | 14 4", "0: 8 11", "13: 14 3 | 1 12", "15: 1 | 14", "17: 14 2 | 1 7", "23: 25 1 | 22 14", "28: 16 1", "4: 1 1", "20: 14 14 | 1 15", "3: 5 14 | 16 1", "27: 1 6 | 14 18", "14: \"b\"", "21: 14 1 | 1 14", "25: 1 1 | 1 14", "22: 14 14", "8: 42 | 42 8", "26: 14 22 | 1 20", "18: 15 15", "7: 14 5 | 1 21", "24: 14 1", "29: 1","30: 1","32: 1","33: 1","34: 1","35: 1", "36: 1", "37: 1", "38: 1", "39: 1", "40: 1", "41: 1","", "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa", "bbabbbbaabaabba", "babbbbaabbbbbabbbbbbaabaaabaaa", "aaabbbbbbaaaabaababaabababbabaaabbababababaaa", "bbbbbbbaaaabbbbaaabbabaaa", "bbbababbbbaaaaaaaabbababaaababaabab", "ababaaaaaabaaab", "ababaaaaabbbaba", "baabbaaaabbaaaababbaababb", "abbbbabbbbaaaababbbbbbaaaababb", "aaaaabbaabaaaaababaa", "aaaabbaaaabbaaa", "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa", "babaaabbbaaabaababbaabababaaab", "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"]





--createParser :: [String] -> Int -> ParserList -> ParserList
--createParser s n l = if isBuilt l n then l
    --else if splitOn ' ' (s!!n) |> (!!1) |> (!!0) |> (=='"') then (n,symbol (splitOn '"' (s!!n) |> (!!1))):l 
    --else if exists (=='|') (s!!n) then ((n, ((combineParsers p1 p2) <|> (combineParsers p3 p4)) ):l')  where
        --parts = splitOn ' ' (s!!n)        
        --l' = createParser s (read (parts!!1)) |> createParser s (read (parts!!2)) |> createParser s (read (parts!!3)) |> --createParser s (read (parts!!4))
--        p1 = getParser l' (read (parts!!1))
        --p2 = getParser l' (read (parts!!2))
        --p3 = getParser l' (read (parts!!3))
        --p4 = getParser l' (read (parts!!4))
    --else (n, combineParsers p1 p2):l' where         
        --parts = splitOn ' ' (s!!n)        
        --l' = createParser s (read (parts!!1)) (createParser s (read (parts!!2)) l)
        --p1 = getParser l' (read (parts!!1))
        --p2 = getParser l' (read (parts!!2))
        
--getParser :: [String] -> Int -> ParserList -> Parser String
--getParser s n l = case find (\(a,b)->a==n) l of 
    --Just (n,p) -> p
    --Nothing -> createParser s n l
--
--isBuilt :: ParserList -> Int ->  Bool
--isBuilt l n = find (\(a,b)->a==n) l |> isJust

        