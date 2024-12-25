import Data.List (find)
import Data.Map qualified as M
import Data.Maybe
import Debug.Trace (trace)

main = readFile "input15.txt" >>= print . solve . parse

c2dir '^' = (0, -1)
c2dir '>' = (1, 0)
c2dir '<' = (-1, 0)
c2dir 'v' = (0, 1)

move (dx, dy) (x, y) = (x + dx, y + dy)

swap :: (Ord k) => k -> k -> M.Map k a -> M.Map k a
swap p1 p2 dict = M.insert p2 (dict M.! p1) (M.insert p1 (dict M.! p2) dict)

toGrid xs = do
  (y, row) <- zip [0 ..] xs
  (x, c) <- zip [0 ..] row
  case c of
    'O' -> [((2 * x, y), '['), ((2 * x + 1, y), ']')]
    '@' -> [((2 * x, y), '@'), ((2 * x + 1, y), '.')]
    _ -> [((2 * x, y), c), ((2 * x + 1, y), c)]

parse input = (M.insert start '.' $ M.fromList grid, concat ins, start)
  where
    (warehouse, ins) = break null $ lines input
    grid = toGrid warehouse
    Just (start, '@') = find ((== '@') . snd) grid

solve (warehouse, ins, start) =
  score $ foldl step (warehouse, start) $ map c2dir ins
  where
    score = sum . map (\(x, y) -> x + y * 100) . map fst . filter ((== '[') . snd) . M.toList . fst

step :: (M.Map (Int, Int) Char, (Int, Int)) -> (Int, Int) -> (M.Map (Int, Int) Char, (Int, Int))
step (warehouse, pos) dir =
  let pos' = move dir pos
   in case warehouse M.! pos' of
        '#' -> (warehouse, pos)
        '.' -> (warehouse, pos')
        '@' -> (warehouse, pos')
        '[' ->
          fromMaybe (warehouse, pos) $
            case dir of
              (1, 0) -> (,pos') <$> pushX pos dir warehouse
              (-1, 0) -> error "[<"
              _ -> (,pos') <$> pushY pos dir warehouse
        ']' ->
          fromMaybe (warehouse, pos) $
            case dir of
              (1, 0) -> error "]<"
              (-1, 0) -> (,pos') <$> pushX pos dir warehouse
              _ -> (,pos') <$> pushY pos dir warehouse

pushX :: (Int, Int) -> (Int, Int) -> M.Map (Int, Int) Char -> Maybe (M.Map (Int, Int) Char)
pushX pos dir warehouse =
  let pos' = move dir pos
   in case M.lookup pos' warehouse of
        Just '.' -> Just $ swap pos pos' warehouse
        Just '#' -> Nothing
        Just c | c `elem` "[]" -> swap pos pos' <$> pushX pos' dir warehouse
        Nothing -> Nothing

pushY :: (Int, Int) -> (Int, Int) -> M.Map (Int, Int) Char -> Maybe (M.Map (Int, Int) Char)
pushY pos dir warehouse =
  let pos' = move dir pos
   in case M.lookup pos' warehouse of
        Just '.' -> Just $ swap pos pos' warehouse
        Just '#' -> Nothing
        Nothing -> Nothing
        Just ']' -> do
          w' <- pushY pos' dir warehouse
          w'' <- pushY (move (c2dir '<') pos') dir w'
          return $ swap pos pos' w''
        Just '[' -> do
          w' <- pushY pos' dir warehouse
          w'' <- pushY (move (c2dir '>') pos') dir w'
          return $ swap pos pos' w''
