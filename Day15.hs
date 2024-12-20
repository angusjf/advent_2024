import Data.List (find)
import Data.Map qualified as M
import Data.Maybe

main = readFile "test15.txt" >>= print . solve . parse

-- main = readFile "test15.txt" >>= putStrLn . vis . parse

-- main = readFile "input15.txt" >>= print . solve . parse

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
          case dir of
            (0, 1) ->
              case pushLeft warehouse pos of
                Nothing -> (warehouse, pos)
                Just warehouse' -> (warehouse', left pos)

left (x, y) = (x - 1, y)

swap :: (Ord k) => k -> k -> M.Map k a -> M.Map k a
swap p1 p2 dict = M.insert p2 (dict M.! p1) (M.insert p1 (dict M.! p2) dict)

pushLeft :: M.Map (Int, Int) Char -> (Int, Int) -> Maybe (M.Map (Int, Int) Char)
pushLeft warehouse (x, y) =
  case warehouse M.! (x - 1, y) of
    '.' -> Just (swap (x, y) (x - 1, y) warehouse)
    ']' ->
      pushLeft warehouse (x - 2, y) >>= \warehouse' -> Just (swap (x, y) (x - 1, y) warehouse')
    '#' ->
      Nothing

pushUp :: M.Map (Int, Int) Char -> (Int, Int) -> Maybe (M.Map (Int, Int) Char)
pushUp warehouse (x, y) =
  case warehouse M.! (x, y - 1) of
    '.' -> Just (swap (x, y) (x, y - 1) warehouse)
    ']' ->
      pushUp warehouse (x - 2, y) >>= \warehouse' -> Just (swap (x, y) (x - 1, y) warehouse')
    '#' -> Nothing

-- findEmptySpace warehouse dir pos =
--   case warehouse M.! pos of
--     '.' -> Just pos
--     '@' -> Just pos
--     '#' -> Nothing
--     'O' -> findEmptySpace warehouse dir (move dir pos)
