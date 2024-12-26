import Data.List (find, nub, partition)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Debug.Trace (trace)

-- main = readFile "test20.txt" >>= print . solve . parse

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

-- solve :: ((Int, Int), (Int, Int), [(Int, Int)]) -> [Int]
solve (start, end, allWalls) = filter (>= 100) $ map ((-) total) $ map (shortestPath start end . S.fromList) (map (++ exterior) $ remove1 interior)
  where
    (exterior, interior) = partition (\(x, y) -> x == 0 || x == mx || y == 0 || y == my) allWalls
    mx = maximum $ map fst allWalls
    my = maximum $ map snd allWalls
    total = shortestPath start end (S.fromList allWalls)

remove1 (x : xs) = xs : (map (x :) (remove1 xs))
remove1 [] = []

shortestPath start end walls =
  dijkstra
    (\(x, y) -> map (,1) $ filter (`S.notMember` walls) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)])
    start
    M.! end

dijkstra :: (Ord a) => (a -> [(a, Int)]) -> a -> M.Map a Int
dijkstra step start = go (S.singleton (0, start)) M.empty
  where
    go q d = case S.minView q of
      Nothing -> d
      Just ((dist, n), q') ->
        if M.member n d
          then go q' d
          else go (foldr S.insert q' [(dist + w, x) | (x, w) <- step n, x `M.notMember` d]) (M.insert n dist d)
