import Data.List (find, nub)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
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

tally :: (Ord x) => [x] -> [(x, Int)]
tally = M.toList . foldl (\m k -> M.insertWith (+) k 1 m) M.empty

solve (start, end, allWalls) = shortestPath start end (S.fromList allWalls)

shortestPath start end walls = flood start (== end) (\(x, y) -> filter (`S.notMember` walls) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)])

flood :: (Ord a) => a -> (a -> Bool) -> (a -> [a]) -> S.Set a
flood seed cond step = go (Seq.singleton seed) S.empty
  where
    go (current Seq.:<| queue) visited
      | cond current = visited
      | current `S.member` visited = go queue visited
      | otherwise =
          let neighbors = filter (`S.notMember` visited) (step current)
              newQueue = queue Seq.>< Seq.fromList neighbors
           in go newQueue (S.insert current visited)
