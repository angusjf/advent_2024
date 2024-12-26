import Control.Exception (assert)
import Data.List (find)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, isNothing, maybeToList)
import Data.Set qualified as S
import Debug.Trace (trace)

-- main = readFile "test16.txt" >>= print . solve . parse

main = readFile "input16.txt" >>= print . solve . parse

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

solve input = (costs M.! (N, end), S.size $ seats paths (== (E, start)) (N, end))
  where
    costs = M.map fst pathsAndCosts
    paths = M.map snd pathsAndCosts
    pathsAndCosts = dijkstra step (E, start)
    Just (start, _) = find ((== 'S') . snd) input
    Just (end, _) = find ((== 'E') . snd) input
    walls = S.fromList $ map fst $ filter ((== '#') . snd) input

    step :: (Dir, (Int, Int)) -> [((Dir, (Int, Int)), Int)]
    step (dir, pos) = filter ((`S.notMember` walls) . snd . fst) [((dir, move dir pos), 1), ((cw dir, pos), 1000), ((ccw dir, pos), 1000)]

dijkstra :: (Ord a, Show a) => (a -> [(a, Int)]) -> a -> M.Map a (Int, [a])
dijkstra step start = go (S.singleton (0, (start, Nothing))) M.empty
  where
    go q d = case S.minView q of
      Nothing -> d
      Just ((dist, (n, mPrev)), q') ->
        go
          ( foldr
              S.insert
              q'
              [ (dist + w, (x, Just n))
              | (x, w) <- step n,
                case M.lookup x d of
                  Just (oldDist, _) -> dist + w <= oldDist
                  Nothing -> True
              ]
          )
          ( M.insertWith
              ( \(newDist, newPrev) (oldDist, oldPrevs) ->
                  case compare newDist oldDist of
                    EQ -> (newDist, newPrev ++ oldPrevs)
                    LT -> (newDist, newPrev)
                    GT -> (oldDist, oldPrevs)
              )
              n
              (dist, maybeToList mPrev)
              d
          )

seats :: (Ord a, Ord z) => M.Map (z, a) [(z, a)] -> ((z, a) -> Bool) -> (z, a) -> S.Set a
seats _ done from | done from = S.singleton (snd from)
seats paths done from = S.insert (snd from) $ S.unions $ map (seats paths done) $ paths M.! from
