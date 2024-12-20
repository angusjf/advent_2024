import Data.List (find, nub)
import Data.Map qualified as M
import Data.Set qualified as S
import Debug.Trace (trace)

main = readFile "input20.txt" >>= print . solve . parse

parse :: String -> ((Int, Int), (Int, Int), [(Int, Int)])
parse input = (start, end, map fst $ filter ((== '#') . snd) grid)
  where
    grid = toGrid (lines input)
    Just (start, 'S') = find ((== 'S') . snd) grid
    Just (end, 'E') = find ((== 'E') . snd) grid

toGrid xs = do
  (y, row) <- zip [0 ..] xs
  (x, c) <- zip [0 ..] row
  return ((x, y), c)

neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

tally :: (Ord x) => [x] -> [(x, Int)]
tally = M.toList . foldl (\m k -> M.insertWith (+) k 1 m) M.empty

solve :: ((Int, Int), (Int, Int), [(Int, Int)]) -> [Int]
solve (start, end, allWalls) = map (\walls -> shortestPath start end (S.fromList walls)) (deleteOneFrom allWalls)

deleteOneFrom xs = map (\index -> map snd $ filter (\(i, x) -> i /= index) enum) [0 .. (length xs) - 1]
  where
    enum = zip [0 ..] xs

shortestPath start end walls = dfs start (== end) (\pos -> filter (`S.notMember` walls) $ neighbors pos)

dfs :: (Show a, Ord a) => a -> (a -> Bool) -> (a -> [a]) -> Int
dfs seed cond step = go [(seed, 0)] S.empty
  where
    go ((v, n) : q) visited
      | cond v = n
      | otherwise = go (nub $ q ++ [(w, 1 + n) | w <- step v, w `S.notMember` visited]) (S.insert v visited)
