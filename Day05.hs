import qualified Data.Map as M
import Data.List (find)

-- main = readFile "test05.txt" >>= print . solve . parse
main = readFile "input05.txt" >>= print . solve . parse

parse input = (map parseRule rules, map parseUpdate updates)
    where (rules, "":updates) = break (== "") (lines input)

parseRule :: String -> (Int, Int)
parseRule input = (read a, read b)
  where (a, '|':b) = break (== '|') input

parseUpdate :: String -> [Int]
parseUpdate = map read . words . map (\c -> if c == ',' then ' ' else c)

-- solve :: ([(Int, Int)], [[Int]]) -> Int
solve (rules, updates) = sum $ map middle $ map (\update -> applyRepeatedly pass update rules) $ filter (\update -> not (all (passes update) rules)) updates

applyRepeatedly makepass update [] = update
applyRepeatedly makepass update (rule:more) = applyRepeatedly makepass (makepass update rule) more

passes :: [Int] -> (Int, Int) -> Bool
passes update (before, after) =
    case (M.lookup before order, M.lookup after order) of
      (Just x, Just y) -> x < y
      _ -> True
  where order = M.fromList (zip update [1..])

middle :: [Int] -> Int
middle xs = xs !! ((length xs) `div` 2)

pass :: [Int] -> (Int, Int) -> [Int]
pass [] _ = []
pass (x:xs) (a, b) = 
  if x == b then
    case find (\(_, item) -> item == a) (zip [1..] xs) of
      Just (i, y) -> pass ((take i xs) ++ [x] ++ (drop i xs)) (a, b)
      Nothing -> x : (pass xs (a, b))
  else
    x : (pass xs (a, b))
