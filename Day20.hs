import Data.List (find, nub, partition)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Debug.Trace (trace)

(main, msMin, cheats) =
  -- (readFile "test20.txt" >>= print . tally . solve . parse, 50, 20)

  (readFile "input20.txt" >>= print . length . solve . parse, 100, 20)

parse :: String -> ((Int, Int), (Int, Int), S.Set (Int, Int))
parse input = (start, end, S.fromList $ map fst $ filter ((== '#') . snd) grid)
  where
    grid = toGrid (lines input)
    Just (start, 'S') = find ((== 'S') . snd) grid
    Just (end, 'E') = find ((== 'E') . snd) grid

toGrid xs = do
  (y, row) <- zip [0 ..] xs
  (x, c) <- zip [0 ..] row
  return ((x, y), c)

tally :: (Ord x) => [x] -> [(x, Int)]
tally = M.toList . foldl (\m k -> M.insertWith (+) k 1 m) M.empty

solve (start :: (Int, Int), end :: (Int, Int), walls :: S.Set (Int, Int)) =
  let path = run start (== end) step S.empty -- length: 85
      step (x, y) = filter (`S.notMember` walls) $ [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
   in path `seq`
        [ savings
        | (start, i) <- zip path [0 ..],
          (end, j) <- zip path [0 ..],
          j > i,
          let skipped = j - i,
          let used = dist start end,
          let savings = skipped - used,
          used <= cheats,
          savings >= msMin
        ]

dist (a, b) (c, d) = abs (a - c) + abs (b - d)

run :: (Ord a, Show a) => a -> (a -> Bool) -> (a -> [a]) -> S.Set a -> [a]
run a done step seen =
  a
    : if done a
      then []
      else run (unvisted (step a) seen) done step (S.insert a seen)

unvisted :: (Ord a, Show a) => [a] -> S.Set a -> a
unvisted xs s =
  case filter (`S.notMember` s) xs of
    [a] -> a
    z -> error $ show z
