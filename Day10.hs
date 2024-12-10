import Data.Map qualified as M
import Data.Set qualified as S
import Debug.Trace

-- main = readFile "test10.txt" >>= print . solve . parse

main = readFile "input10.txt" >>= print . solve . parse

parse :: String -> (M.Map (Int, Int) Int, [(Int, Int)])
parse input = (M.fromList grid, zeros)
  where
    grid = concatMap (\(y, zz) -> map (\(x, c) -> ((x, y), read [c])) zz) $ zip [0 ..] $ (map (zip [0 ..])) $ lines input
    zeros = map fst $ filter (\(p, n) -> n == 0) grid

solve (grid, zeros) = sum $ map (score grid S.empty . S.singleton) zeros

score :: M.Map (Int, Int) Int -> S.Set (Int, Int) -> S.Set (Int, Int) -> Int
score grid _ toVisit | S.null toVisit = 0
score grid visited toVisit =
  let (pos, toVisit') = S.deleteFindMin toVisit
      visited' = S.insert pos visited
   in if S.member pos visited
        then
          score grid visited' toVisit'
        else case M.lookup pos grid of
          Nothing -> error "score grid visited' toVisit'"
          Just 9 -> 1 + score grid visited' toVisit'
          Just n -> score grid visited' $ S.union toVisit' $ S.fromList $ filter (\nx -> M.lookup nx grid == Just (n + 1)) $ next pos

next (x, y) =
  [ (x - 1, y),
    (x + 1, y),
    (x, y - 1),
    (x, y + 1)
  ]
