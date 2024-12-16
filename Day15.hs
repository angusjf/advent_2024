import Data.List (find)
import Data.Map qualified as M

-- main = readFile "test15.txt" >>= print . solve . parse

-- main = readFile "test15.txt" >>= putStrLn . vis . parse

main = readFile "input15.txt" >>= print . solve . parse

dir '^' = (0, -1)
dir '>' = (1, 0)
dir '<' = (-1, 0)
dir 'v' = (0, 1)

move (dx, dy) (x, y) = (x + dx, y + dy)

draw (mx, my) grid =
  ((('+' :) $ take mx $ repeat '-') ++ "+")
    : [ '|'
          : [ grid M.! (x, y)
            | x <- [0 .. mx - 1]
            ]
          ++ ['|']
      | y <- [0 .. my - 1]
      ]
    ++ [(('+' :) $ take mx $ repeat '-') ++ "+"]

toGrid xs = do
  (y, row) <- zip [0 ..] xs
  (x, c) <- zip [0 ..] row
  return ((x, y), c)

parse input = (M.fromList grid, concat ins, start)
  where
    (warehouse, ins) = break null $ lines input
    grid = toGrid warehouse
    Just (start, '@') = find ((== '@') . snd) grid

solve (warehouse, ins, start) =
  sum $
    map (\(x, y) -> x + y * 100) $
      map fst $
        filter ((== 'O') . snd) $
          -- unlines $
          --   draw (8, 8) $
          M.toList $
            fst $
              foldl step (warehouse, start) (map dir ins)

vis (warehouse, ins, start) =
  unlines $
    draw (8, 8) $
      fst $
        foldl step (warehouse, start) (map dir ins)

step :: (M.Map (Int, Int) Char, (Int, Int)) -> (Int, Int) -> (M.Map (Int, Int) Char, (Int, Int))
step (warehouse, pos) dir =
  let pos' = move dir pos
   in case warehouse M.! pos' of
        '#' -> (warehouse, pos)
        '.' -> (warehouse, pos')
        '@' -> (warehouse, pos')
        'O' ->
          case findEmptySpace warehouse dir pos' of
            Nothing -> (warehouse, pos)
            Just empty -> (M.insert empty 'O' (M.insert pos' '.' warehouse), pos')

findEmptySpace warehouse dir pos =
  case warehouse M.! pos of
    '.' -> Just pos
    '@' -> Just pos
    '#' -> Nothing
    'O' -> findEmptySpace warehouse dir (move dir pos)
