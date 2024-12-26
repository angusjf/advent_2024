import Data.List (nub)
import Data.Set qualified as S
import Debug.Trace

-- main = readFile "input18.txt" >>= print . solve . parse

main = readFile "test18.txt" >>= print . solve . parse

parse :: String -> [(Int, Int)]
parse input = map read $ map (\l -> '(' : l ++ ")") $ lines input

neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

solve points = head [n | n <- [12 ..], s n points == Nothing]

s n points = trace (show n) $ dfs (0, 0) (== end) (filter (inRange ((0, 0), end)) . filter (`S.notMember` mem) . neighbors)
  where
    mem = S.fromList (take n points)
    end = let (x, y) = S.findMax mem in (max x y, max x y)

inRange ((xMin, yMin), (xMax, yMax)) (x, y) = x >= xMin && x <= xMax && y >= yMin && y <= yMax

dfs :: (Show a, Ord a) => a -> (a -> Bool) -> (a -> [a]) -> Maybe Int
dfs seed cond step = go [(seed, 0)] S.empty
  where
    go [] _ = Nothing
    go ((v, n) : q) visited
      | cond v = Just n
      | otherwise = go (nub $ q ++ [(w, 1 + n) | w <- step v, w `S.notMember` visited]) (S.insert v visited)
