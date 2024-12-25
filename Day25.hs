import Data.List (transpose)
import Data.Set qualified as S

-- main = readFile "test25.txt" >>= print . solve . parse

main = readFile "input25.txt" >>= print . solve . parse

parse input =
  let xs = map transpose $ split $ lines input
      keys ((c : _) : _) = c == '#'
      noDots = map (filter (== '#'))
   in (map noDots $ (filter keys) xs, map noDots $ (filter (not . keys)) xs)

split xs =
  case break null xs of
    (x, []) -> [x]
    (x, [] : more) -> x : split more

heights = map (pred . length)

solve (keys, locks) = length [(k, l) | k <- map heights keys, l <- map heights locks, fits k l]

fits key lock = all id $ zipWith (\k l -> k + l < 6) key lock
