main = readFile "input01.txt" >>= print . solve . parse

solve (lefts, rights) = sum $ map similarity lefts
  where
    similarity left = left * length (filter (== left) rights)

parse :: String -> ([Int], [Int])
parse input = (map fst pairs, map snd pairs)
  where
    pairs = map parseLine (lines input)

parseLine line = (read a, read b)
  where
    (a, b) = break (== ' ') line
