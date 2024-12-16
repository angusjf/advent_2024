import Data.Function (on)
import Data.List (groupBy, intersperse, maximumBy, minimumBy, nub, sortBy)
import Data.Map ((!))
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Set qualified as S

-- main = readFile "input12.txt" >>= print . solve . parse

main = readFile "test12.txt" >>= putStrLn . solve . parse

parse = toGrid . lines

toGrid xs = do
  (y, row) <- zip [0 ..] xs
  (x, c) <- zip [0 ..] row
  return ((x, y), c)

solve points =
  -- sum $
  unlines $ map score $ zones grid
  where
    grid = M.fromList points

zones :: M.Map (Int, Int) Char -> [(Char, S.Set (Int, Int))]
zones grid =
  case M.minViewWithKey grid of
    Just ((pos, char), grid') ->
      let positions = zone grid' (pos, char)
       in (char, positions) : zones (M.withoutKeys grid positions)
    Nothing -> []

neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

zone :: M.Map (Int, Int) Char -> ((Int, Int), Char) -> S.Set (Int, Int)
zone grid (pos, c) = allAdjacent (S.singleton pos) (S.fromList $ map fst $ filter ((== c) . snd) $ M.toList grid)

allAdjacent :: S.Set (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
allAdjacent ok toCheck =
  case S.partition (isAdjacentToAny ok) toCheck of
    (new, old) | S.null new || S.null old -> S.union ok new
    (new, old) -> allAdjacent (S.union ok new) old

isAdjacentToAny ok pos = any (\pos -> S.member pos ok) (neighbors pos)

draw (mx, my) grid =
  ((('+' :) $ take mx $ repeat '-') ++ "+")
    : [ '|'
          : [ if S.member (x, y) grid then '#' else ' '
            | x <- [-1 .. mx - 1]
            ]
          ++ ['|']
      | y <- [-2 .. my - 2]
      ]
    ++ [(('+' :) $ take mx $ repeat '-') ++ "+"]

-- score :: (Char, S.Set (Int, Int)) -> ?
score (c, points) = [c] ++ ":\n" ++ (unlines $ draw (11, 11) $ nEdges points)

-- (c, length points, nEdges points)

nEdges points = convolve2x2Map (10, 10) leftEdgesKernel points

leftEdgesKernel = S.fromList [(0, 0), (1, 0)]

convolve2x2Map :: (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
convolve2x2Map (maxX, maxY) kernel matrix =
  S.fromList $
    catMaybes
      [ if any
          id
          [ S.member (kx, ky) kernel && S.member (x + kx, y + ky) matrix
          | kx <- [0, 1],
            ky <- [0, 1]
          ]
          then Just (x, y)
          else Nothing
      | x <- [-2 .. maxX + 1],
        y <- [-2 .. maxY + 1]
      ]
