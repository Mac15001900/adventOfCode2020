import MyUtils

findSmaller :: Integer -> [Integer] -> [Integer]
findSmaller n = filter (\x -> x<=n)

findLarger :: Integer -> [Integer] -> [Integer]
findLarger n = filter (\x -> x>=n)

process1 :: [String] -> [Integer]
process1 file = map read file

--          Target     Candidates   Number 
canSumTo :: Integer -> [Integer] -> Integer -> Bool
canSumTo _ []     _ = False
canSumTo a (x:xs) b = b+x==a || canSumTo a xs b

process2 :: [Integer] -> [[Integer]]
process2 xs = xs |> filter (<1000) |> filter (canSumTo 2000 (filter (>1000) xs)) |> map (\a->[a,2000-a])

process3 :: [Integer] -> [Integer]
process3 xs = xs |> filter (\x -> (length (filter (canSumTo (2000-x) xs) xs)) > 0)