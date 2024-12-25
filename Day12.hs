import Data.Function (on)
import Data.List (groupBy, intersperse, maximumBy, minimumBy, nub, partition, singleton, sortBy)
import Data.Map ((!))
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Set qualified as S
import Debug.Trace (trace)

main = readFile "input12.txt" >>= print . solve . parse

-- main = readFile "test12.txt" >>= print . solve . parse

parse = toGrid . lines

toGrid xs = do
  (y, row) <- zip [0 ..] xs
  (x, c) <- zip [0 ..] row
  return ((x, y), c)

solve points =
  sum
    $ map
      ( \(_, points) ->
          let area = length points
              perimeter = length $ concat $ M.elems $ perim points
           in area * sides points
      )
    $ zones
    $ M.fromList points

groupAdj :: S.Set (Int, Int) -> [S.Set (Int, Int)]
groupAdj all =
  case S.minView all of
    Just (x, _) ->
      let adj = allAdjacent (S.singleton x) all
       in adj : groupAdj (all S.\\ adj)
    Nothing -> []

sides points = sum $ map (length . groupAdj . S.fromList) $ M.elems p
  where
    p = perim points
    adj (x0, y0) (x1, y1) = x0 == x1 || y0 == y1

zones :: M.Map (Int, Int) Char -> [(Char, S.Set (Int, Int))]
zones grid =
  case M.minViewWithKey grid of
    Just ((pos, char), grid') ->
      let positions = zone grid' (pos, char)
       in (char, positions) : zones (M.withoutKeys grid positions)
    Nothing -> []

dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

zone :: M.Map (Int, Int) Char -> ((Int, Int), Char) -> S.Set (Int, Int)
zone grid (pos, c) = allAdjacent (S.singleton pos) (S.fromList $ map fst $ filter ((== c) . snd) $ M.toList grid)

allAdjacent :: S.Set (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
allAdjacent ok toCheck =
  case S.partition (isAdjacentToAny ok) toCheck of
    (new, old) | S.null new || S.null old -> S.union ok new
    (new, old) -> allAdjacent (S.union ok new) old

(x, y) +: (a, b) = (x + a, b + y)

isAdjacentToAny ok pos = any (\pos -> S.member pos ok) (neighbors pos)
  where
    neighbors pos = map (+: pos) dirs

perim :: S.Set (Int, Int) -> M.Map (Int, Int) [(Int, Int)]
perim points = go [S.findMin points] M.empty S.empty
  where
    go :: [(Int, Int)] -> M.Map (Int, Int) [(Int, Int)] -> S.Set (Int, Int) -> M.Map (Int, Int) [(Int, Int)]
    go [] acc _ = acc
    go (pos : q) acc visited =
      let optim xy = xy `notElem` q && S.notMember xy visited
          onShape xy = S.member xy points
          (ok, ko) = partition (onShape . snd) $ filter (optim . snd) $ zipWith (\p v -> (v, p +: v)) (repeat pos) dirs

          acc' = M.unionWith (++) acc $ M.fromListWith (++) $ map (\(k, v) -> (k, [v])) ko
       in go (q ++ map snd ok) acc' (S.insert pos visited)
