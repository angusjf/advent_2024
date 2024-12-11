import Data.Map qualified as M

main = readFile "input11.txt" >>= print . solve . parse

-- main = readFile "test11.txt" >>= print . solve . parse

parse :: String -> [String]
parse = words

solve ns = sum $ map snd $ M.toList $ (!! 75) $ iterate update ini
  where
    ini = M.fromListWith (+) $ zip ns (repeat 1)

update :: M.Map String Int -> M.Map String Int
update state = M.fromListWith (+) $ concatMap (\(x, tally) -> zip (step x) (repeat tally)) $ M.toList state

step "0" = ["1"]
step n =
  case split n of
    Just [a, b] -> [a, b]
    Nothing -> [show $ (* 2024) $ ((read n) :: Integer)]

split n
  | l `mod` 2 == 0 =
      Just
        [ take (l `div` 2) n,
          case dropWhile (== '0') $ drop (l `div` 2) n of
            [] -> "0"
            x -> x
        ]
  where
    l = length n
split _ = Nothing
