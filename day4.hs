import MyUtils
import Text.Regex.PCRE

run :: Show a => ([String] -> a) -> IO ()
run = runOnFile "input4.txt"

--Part 1--

fieldsNeeded :: [String]
fieldsNeeded = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

splitToPassports :: [String] -> [String]
splitToPassports xs = xs |> splitOn "" |> map (joinWith " ")

fieldsPresent :: String -> [String]
fieldsPresent xs = xs |> splitOn ' ' |> map (splitOn ':') |> map (take 1) |> map concat

hasAllFields :: [String] -> Bool
hasAllFields fields = fieldsNeeded |> filter (\x -> not $ elem x fields) |> length |> (==0)

part1 :: [String] -> Int
part1 input = input |> splitToPassports |> map fieldsPresent |> filter hasAllFields |> length

--Part 2--

getFields :: String -> [[String]]
getFields xs = xs |> splitOn ' ' |> map (splitOn ':')

isFieldValid :: [String] -> Bool
isFieldValid ["byr",value] = valueBetween (1920,2002) (read value)
isFieldValid ["iyr",value] = valueBetween (2010,2020) (read value)
isFieldValid ["eyr",value] = valueBetween (2020,2030) (read value)
isFieldValid ["hgt",a:b:c:"cm"] = valueBetween (150,193) (read [a,b,c])
isFieldValid ["hgt",a:b:"in"] = valueBetween (59,76) (read [a,b])
isFieldValid ["hcl",'#':color] = (filter (\c-> elem c valid) color|> length) == 6 && (length color) == 6 
    where valid = "0123456789abcdef"
isFieldValid ["ecl",value] = elem value ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isFieldValid ["pid",value] = (value |> map (\x-> elem x "0123456789") |> and) && (length value) == 9
isFieldValid ["cid",_] = True
isFieldValid _ = False

part2 :: [String] -> Int
part2 input = input |> splitToPassports |> filter (hasAllFields . fieldsPresent) |> map getFields |> map (map isFieldValid) |> filter and |> length

--Debug--

getValidFields :: [String] -> [[String]]
getValidFields i = i |> splitToPassports |> map getFields |> concat |> filter isFieldValid

getInValidFields :: [String] -> [[String]]
getInValidFields i = i |> splitToPassports |> map getFields |> concat |> filter (not . isFieldValid)


