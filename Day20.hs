import Data.List (find, nub, partition)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Debug.Trace (trace)

main = readFile "test20.txt" >>= print . solve . parse

-- main = readFile "input20.txt" >>= print . solve . parse

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

solve (start, end, allWalls) = path prevs (end, Used)
  where
    (base, _) = nocheat start end (S.fromList allWalls)
    distsAndPrevs = shortestPath start end allWalls
    dists = M.map fst distsAndPrevs
    prevs = M.map snd distsAndPrevs

path :: (Ord a) => M.Map a (Maybe a) -> a -> [a]
path prevs x =
  x
    : case prevs M.! x of
      Just x -> path prevs x
      Nothing -> []

data Super = Unused | NoClip Int | Used deriving (Eq, Ord, Show)

shortestPath start end allWalls =
  dijkstra
    ( \((x, y), super) ->
        case super of
          Unused ->
            (((x, y), NoClip 2), 0)
              : ( map (,1) $
                    map (,Unused) $
                      filter (`S.notMember` exterior) $
                        filter (`S.notMember` interior) $
                          [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                )
          NoClip 0 -> [(((x, y), Used), 0)]
          NoClip n ->
            (((x, y), Used), 0)
              : ( map (,1) $
                    map (,NoClip (n - 1)) $
                      filter (`S.notMember` exterior) $
                        [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                )
          Used ->
            map (,1) $
              map (,Used) $
                filter (`S.notMember` exterior) $
                  filter (`S.notMember` interior) $
                    [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    )
    (start, Unused)
  where
    exterior = S.fromList ext
    interior = S.fromList int
    (ext, int) = partition (\(x, y) -> x == 0 || x == mx || y == 0 || y == my) allWalls
    mx = maximum $ map fst allWalls
    my = maximum $ map snd allWalls

nocheat start end walls =
  dijkstra
    (\(x, y) -> map (,1) $ filter (`S.notMember` walls) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)])
    start
    M.! end

dijkstra :: (Ord a) => (a -> [(a, Int)]) -> a -> M.Map a (Int, Maybe a)
dijkstra step start = go (S.singleton (0, start, Nothing)) M.empty
  where
    go q d = case S.minView q of
      Nothing -> d
      Just ((dist, n, mPrev), q') ->
        if M.member n d
          then go q' d
          else
            go
              (foldr S.insert q' [(dist + w, x, Just n) | (x, w) <- step n, x `M.notMember` d])
              (M.insert n (dist, mPrev) d)
