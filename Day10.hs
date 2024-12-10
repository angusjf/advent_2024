import Data.Map qualified as M

main = readFile "input10.txt" >>= print . solve . parse

parse :: String -> (M.Map (Int, Int) Int, [(Int, Int)])
parse input = (M.fromList grid, zeros)
  where
    grid = map (\(p, c) -> (p, read [c])) $ toGrid (lines input)
    zeros = map fst $ filter (\(p, n) -> n == 0) grid

toGrid xs = do
  (y, row) <- zip [0 ..] xs
  (x, c) <- zip [0 ..] row
  return ((x, y), c)

solve (grid, zeros) = sum (map score zeros)
  where
    score pos =
      case M.lookup pos grid of
        Just 9 -> 1
        Nothing -> 0
        Just n -> sum $ map score $ filter (\nx -> M.lookup nx grid == Just (n + 1)) $ next pos

next (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
