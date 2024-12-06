import qualified Data.Set as S

main = readFile "input06.txt" >>= print . solve . parse

parse input = (fst $ head $ filter (\(_, c) -> c == '^') grid, S.fromList $ map fst $ filter (\(_, c) -> c == '#') grid, mx, my)
  where grid = concatMap (\(y, xcs) -> map (\(x, c) -> ((x, y), c)) xcs) $ zip [0..] $ map (zip [0..]) $ lines input
        mx = length (lines input !! 0)
        my = length (lines input)

solve (pos, grid, mx, my) = [ (x, y) | x <- [0..mx - 1], y <- [0..my], go pos (0, -1) (S.insert (x, y) grid) mx my S.empty ]

go (x, y) dir grid mx my visited | x < 0 || y < 0 || x >= mx || y >= my = False
go pos dir grid mx my visited | S.member (pos, dir) visited = True
go pos dir grid mx my visited =
  if S.member (move pos dir) grid then
    go pos (turnRight dir) grid mx my visited
  else
    go (move pos dir) dir grid mx my (S.insert (pos, dir) visited)

turnRight (1, 0) = (0, 1)
turnRight (0, 1) = (-1, 0)
turnRight (-1, 0) = (0, -1)
turnRight (0, -1) = (1, 0)

move (x, y) (dx, dy) = (x + dx, y + dy)
