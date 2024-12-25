import Data.List (find)
import Data.Map qualified as M
import Data.Maybe

main = readFile "input15.txt" >>= print . solve . parse

c2dir '^' = (0, -1)
c2dir '>' = (1, 0)
c2dir '<' = (-1, 0)
c2dir 'v' = (0, 1)

move (dx, dy) (x, y) = (x + dx, y + dy)

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
    . foldl step (warehouse, start)
    $ map c2dir ins

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
            case dir of
              (_, 0) -> push pos' dir warehouse
              (0, _) ->
                push pos' dir warehouse
                  >>= let otherHalf = c2dir $ case c of ']' -> '<'; '[' -> '>'
                       in push (move otherHalf pos') dir
