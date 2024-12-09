import Data.Map qualified as M
import Data.Set qualified as S

-- main = readFile "test08.txt" >>= putStrLn . unlines . solve . parse
-- main = readFile "test08_mini.txt" >>= putStrLn . unlines . solve . parse

-- main = readFile "test08.txt" >>= print . solve . parse

main = readFile "input08.txt" >>= print . solve . parse

parse input = ((mx, my), filter (\(c, _) -> c /= '.') grid)
  where
    mx = length $ lines input
    my = length $ head $ lines input
    grid = concatMap (\(y, zz) -> map (\(x, c) -> (c, (x, y))) zz) $ zip [0 ..] $ (map (zip [0 ..])) $ lines input

draw (mx, my) grid antis =
  [ [ case M.lookup (x, y) grid of
        Just c -> c
        Nothing -> if S.member (x, y) antis then '#' else '.'
    | x <- [0 .. mx - 1]
    ]
  | y <- [0 .. my - 1]
  ]

swap (a, b) = (b, a)

solve (max, grid) = S.size antis
  where
    -- draw max (M.fromList $ map swap grid) antis

    antis = S.fromList $ filter (inside max) $ concatMap (\(_, nodes) -> allAntinodes nodes) grouped
    grouped = M.toList $ M.fromListWith (++) $ map (\(c, pos) -> (c, [pos])) grid

allAntinodes xs = concatMap antinodes $ pairs xs

pairs :: [a] -> [(a, a)]
pairs xs = [(a, b) | (i, a) <- e, (j, b) <- e, i > j] where e = zip [0 ..] xs

antinodes ((x1, y1), (x2, y2)) =
  concat
    [ [ (x1 - xd * n, y1 - yd * n),
        (x2 + xd * n, y2 + yd * n)
      ]
    | n <- [0 .. 100000]
    ]
  where
    xd = x2 - x1
    yd = y2 - y1

inside (mx, my) (x, y) = x >= 0 && y >= 0 && x < mx && y < my
