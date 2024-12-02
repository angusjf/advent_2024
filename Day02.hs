main = readFile "input02.txt" >>= print . length . filter id . map (any safe . remove1 . map read . words) . lines

remove1 (x : xs) = xs : (map (x :) (remove1 xs))
remove1 [] = []

safe :: [Int] -> Bool
safe xs = all ok pairs && (all (uncurry (<=)) pairs || all (uncurry (>=)) pairs)
  where
    pairs = zip xs (drop 1 xs)

ok (a, b) = abs (a - b) >= 1 && abs (a - b) <= 3
