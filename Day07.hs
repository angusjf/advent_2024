main = readFile "input07.txt" >>= print . solve . parse

solve input = sum $ map fst $ filter works input

parse :: String -> [(Int, [Int])]
parse = map parseLine . lines
  where
    parseLine line = let (sum, ':' : parts) = break (== ':') line in (read sum, map read $ words parts)

works :: (Int, [Int]) -> Bool
works (sum, p : parts) = go sum p parts

go sum p [] = sum == p
go sum p (x : xs) = go sum (p + x) xs || go sum (p * x) xs || go sum (p ||| x) xs

a ||| b = read $ (show a) ++ show b
