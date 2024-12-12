import Data.List (intersperse, nub)
import Data.Map qualified as M
import Data.Set qualified as S

main = readFile "input12.txt" >>= putStrLn . visualize . parse

-- main = readFile "input12.txt" >>= print . solve . parse

-- main = readFile "test12.txt" >>= print . solve . parse

parse = toGrid . lines

toGrid xs = do
  (y, row) <- zip [0 ..] xs
  (x, c) <- zip [0 ..] row
  return ((x, y), c)

visualize :: [((Int, Int), Char)] -> String
visualize points = unlines $ map (\(c, points) -> ("Zone '" ++ [c] ++ "':\n" ++ (unlines $ draw (mx, my) points c))) $ zones (M.fromList points)
  where
    (mx, my) = maximum $ map fst points

solve points = sum $ map score $ zones grid
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

draw (mx, my) grid c =
  ((('+' :) $ take mx $ repeat '-') ++ "+")
    : [ '|'
          : [ if S.member (x, y) grid then c else ' '
            | x <- [0 .. mx - 1]
            ]
          ++ ['|']
      | y <- [0 .. my - 1]
      ]
    ++ [(('+' :) $ take mx $ repeat '-') ++ "+"]

zone :: M.Map (Int, Int) Char -> ((Int, Int), Char) -> S.Set (Int, Int)
zone grid (pos, c) = allAdjacent (S.singleton pos) (S.fromList $ map fst $ filter ((== c) . snd) $ M.toList grid)

allAdjacent :: S.Set (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
allAdjacent ok toCheck =
  case S.partition (isAdjacentToAny ok) toCheck of
    (new, old) | S.null new || S.null old -> S.union ok new
    (new, old) -> allAdjacent (S.union ok new) old

isAdjacentToAny ok pos = any (\pos -> S.member pos ok) (neighbors pos)

score (c, points) =
  -- (c, length points, perimeter points)
  (length points) * (perimeter points)

perimeter :: S.Set (Int, Int) -> Int
perimeter points = sum $ map (contrib points) (S.toList points)

contrib :: S.Set (Int, Int) -> (Int, Int) -> Int
contrib points point = length $ filter (\n -> S.notMember n points) (neighbors point)
