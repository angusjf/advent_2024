import Data.List (find)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Set qualified as S
import Debug.Trace (trace)

-- main = readFile "test16.txt" >>= print . solve . parse
main = readFile "input20.txt" >>= print . solve . parse

parse input = toGrid (lines input)

toGrid xs = do
  (y, row) <- zip [0 ..] xs
  (x, c) <- zip [0 ..] row
  return ((x, y), c)

data Dir = N | E | S | W deriving (Eq, Ord, Show)

move N (x, y) = (x, y - 1)
move E (x, y) = (x + 1, y)
move S (x, y) = (x, y + 1)
move W (x, y) = (x - 1, y)

cw N = E
cw E = S
cw S = W
cw W = N

ccw = cw . cw . cw

solve input = reconstruct pathL end
  where
    (distL, pathL) = dijkstraWithPaths step (E, start)
    Just (start, _) = find ((== 'S') . snd) input
    Just (end, _) = find ((== 'E') . snd) input
    walls = S.fromList $ map fst $ filter ((== '#') . snd) input

    step :: (Dir, (Int, Int)) -> [((Dir, (Int, Int)), Int)]
    step (dir, pos) = filter ((`S.notMember` walls) . snd . fst) [((dir, move dir pos), 1), ((cw dir, pos), 1000), ((ccw dir, pos), 1000)]

dijkstra :: (Ord a) => (a -> [(a, Int)]) -> a -> M.Map a Int
dijkstra step start = go (S.singleton (0, start)) M.empty
  where
    go q d = case S.minView q of
      Nothing -> d
      Just ((dist, n), q') ->
        if M.member n d
          then go q' d
          else go (foldr S.insert q' [(dist + w, x) | (x, w) <- step n, x `M.notMember` d]) (M.insert n dist d)

dijkstraWithPaths :: (Ord a) => (a -> [(a, Int)]) -> a -> (M.Map a Int, M.Map a a)
dijkstraWithPaths step start = go (S.singleton (0, start)) M.empty M.empty
  where
    go q d p = case S.minView q of
      Nothing -> (d, p)
      Just ((dist, n), q') ->
        if M.member n d
          then go q' d p
          else
            go
              ( foldr
                  S.insert
                  q'
                  [(dist + w, x) | (x, w) <- step n, M.notMember x d]
              )
              (M.insert n dist d)
              ( foldl'
                  ( \acc (x, w) ->
                      if M.notMember x d
                        then M.insert x n acc
                        else acc
                  )
                  p
                  (step n)
              )

reconstruct :: M.Map (Dir, (Int, Int)) (Dir, (Int, Int)) -> (Int, Int) -> [[(Int, Int)]]
reconstruct pathsL from = reconstruct' f [[from]]
  where
    f x = fromMaybe [] $ M.lookup x (M.foldrWithKey (\(_, pos) (_, v) acc -> M.insertWith (++) pos [v] acc) M.empty pathsL)

reconstruct' :: (a -> [a]) -> [[a]] -> [[a]]
reconstruct' back protopaths = concatMap kickback protopaths
  where
    kickback protopath = map (\b -> b : protopath) (back (head protopath))
