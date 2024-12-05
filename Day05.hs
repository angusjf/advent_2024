import Data.List (find, findIndex, sortBy)
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

-- main = readFile "test05.txt" >>= print . solve . parse

main = readFile "input05.txt" >>= print . solve . parse

parse input = (map parseRule rules, map parseUpdate updates)
  where
    (rules, "" : updates) = break (== "") (lines input)

parseRule input = (a, b)
  where
    (a, '|' : b) = break (== '|') input

parseUpdate = words . map (\c -> if c == ',' then ' ' else c)

solve (rules, updates) =
  sum $
    map read $
      map middle $
        map (pass rules) $
          filter (fails rules) updates

middle xs = xs !! ((length xs) `div` 2)

pass :: [(String, String)] -> [String] -> [String]
pass rules update = sortBy (\a b -> if S.member (a, b) rulesSet then LT else GT) update
  where
    rulesSet = S.fromList rules

fails :: [(String, String)] -> [String] -> Bool
fails rules update = any (\(before, after) -> fromMaybe (-1) (indexOf before update) > fromMaybe 99 (indexOf after update)) rules

indexOf a xs = findIndex (== a) xs
