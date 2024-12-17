import Data.List (find)
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Data.Set qualified as S
import Debug.Trace (trace)

-- main = readFile "test16.txt" >>= putStrLn . unlines . draw (15, 15) . S.fromList . map snd . solve . parse
-- main = readFile "input16.txt" >>= print . solve . parse
main = readFile "test16.txt" >>= print . solve . parse

-- main = readFile "debug16.txt" >>= print . solve . parse

parse input = toGrid (lines input)

toGrid xs = do
  (y, row) <- zip [0 ..] xs
  (x, c) <- zip [0 ..] row
  return ((x, y), c)

-- draw (mx, my) grid =
--   ((('+' :) $ take mx $ repeat '-') ++ "+")
--     : [ '|'
--           : [ if S.member (x, y) grid then '>' else '.'
--             | x <- [0 .. mx - 1]
--             ]
--           ++ ['|']
--       | y <- [0 .. my - 1]
--       ]
--     ++ [(('+' :) $ take mx $ repeat '-') ++ "+"]

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

solve :: [((Int, Int), Char)] -> Int
solve input = (M.mapKeysWith min snd $ dijkstra step (E, start)) M.! end
  where
    Just (start, _) = find ((== 'S') . snd) input
    Just (end, _) = find ((== 'E') . snd) input
    walls = S.fromList $ map fst $ filter ((== '#') . snd) input

    step :: (Dir, (Int, Int)) -> [((Dir, (Int, Int)), Int)]
    step (dir, pos) = filter ((`S.notMember` walls) . snd . fst) [((dir, move dir pos), 1), ((cw dir, pos), 1000), ((ccw dir, pos), 1000)]

dijkstra :: (Ord a) => (a -> [(a, Int)]) -> a -> M.Map a Int
dijkstra step start = go (S.singleton (0, start)) M.empty
  where
    go q d = case S.minView q of
      Nothing -> d
      Just ((dist, n), q') ->
        if M.member n d
          then go q' d
          else go (foldr S.insert q' [(dist + w, x) | (x, w) <- step n, x `M.notMember` d]) (M.insert n dist d)
