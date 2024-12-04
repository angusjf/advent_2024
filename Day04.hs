import Data.Map qualified as M

main = readFile "input04.txt" >>= print . solve . parse

parse :: String -> M.Map (Int, Int) Char
parse = M.fromList . gridify . lines

gridify :: [[a]] -> [((Int, Int), a)]
gridify grid =
  concatMap (\(y, row) -> (zipWith (\x c -> ((x, y), c)) [0 ..] row)) $
    zip [0 ..] grid

solve grid = length $ filter (valid grid) $ concatMap possible as
  where
    as = map fst . filter (\(_, c) -> c == 'A') $ M.toList grid

possible :: (Int, Int) -> [[((Int, Int), Char)]]
possible (x, y) =
  map
    ( \(a, b, c, d) ->
        [ ((x + 1, y + 1), a),
          ((x - 1, y + 1), b),
          ((x, y), 'A'),
          ((x + 1, y - 1), c),
          ((x - 1, y - 1), d)
        ]
    )
    [('S', 'S', 'M', 'M'), ('M', 'M', 'S', 'S'), ('S', 'M', 'S', 'M'), ('M', 'S', 'M', 'S')]

valid :: M.Map (Int, Int) Char -> [((Int, Int), Char)] -> Bool
valid grid line = all (\(xy, c) -> M.lookup xy grid == Just c) line
