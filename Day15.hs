import Data.List (find)
import Data.Map qualified as M
import Data.Maybe

main = readFile "input15.txt" >>= print . solve . parse

move '^' (x, y) = (x, y - 1)
move '>' (x, y) = (x + 1, 0)
move '<' (x, y) = (x - 1, 0)
move 'v' (x, y) = (x, y + 1)

swap p1 p2 dict =
  M.insert p2 (dict M.! p1) $
    M.insert p1 (dict M.! p2) $
      dict

toGrid xs = do
  (y, row) <- zip [0 ..] xs
  (x, c) <- zip [0 ..] row
  let [l, r] = case c of
        'O' -> "[]"
        '@' -> "@."
        '#' -> "##"
        '.' -> ".."
  [((2 * x, y), l), ((2 * x + 1, y), r)]

parse input =
  let (warehouse, ins) = break null $ lines input
      grid = toGrid warehouse
      Just (start, '@') = find ((== '@') . snd) grid
   in (M.insert start '.' $ M.fromList grid, concat ins, start)

solve (warehouse, ins, start) =
  sum
    . map (\(x, y) -> x + y * 100)
    . map fst
    . filter ((== '[') . snd)
    . M.toList
    . fst
    $ foldl step (warehouse, start) ins

step (warehouse, pos) dir =
  fromMaybe (warehouse, pos) $
    let pos' = move dir pos
     in (,pos')
          <$> case warehouse M.! pos' of
            '#' -> Nothing
            '.' -> Just warehouse
            _ -> push pos dir warehouse

push pos dir warehouse =
  let pos' = move dir pos
   in swap pos pos'
        <$> case warehouse M.! pos' of
          '.' -> Just warehouse
          '#' -> Nothing
          c ->
            push pos' dir warehouse
              >>= if dir `elem` "<>"
                then return
                else push (move (case c of ']' -> '<'; '[' -> '>') pos') dir
