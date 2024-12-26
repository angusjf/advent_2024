import Data.Maybe (mapMaybe)
import Data.Set qualified as S

main = readFile "input14.txt" >>= putStrLn . solve (101, 103) . parse

parse :: String -> [((Int, Int), (Int, Int))]
parse = map parseLine . lines
  where
    parseLine ('p' : '=' : more) =
      let (px, ',' : more') = break (== ',') more
          (py, ' ' : 'v' : '=' : more'') = break (== ' ') more'
          (vx, ',' : vy) = break (== ',') more''
       in ((read px, read py), (read vx, read vy))

solve room robots =
  unlines $
    [ (\x -> "time " ++ show t ++ ":\n" ++ x) $ unlines $ draw room $ S.fromList $ map (simulate room t) robots | t <- [0 .. 10403]
    ]

simulate (roomSizeX, roomSizeY) t ((x, y), (vx, vy)) = ((x + t * vx) `mod` roomSizeX, (y + t * vy) `mod` roomSizeY)

draw (mx, my) grid =
  ((('+' :) $ take mx $ repeat '-') ++ "+")
    : [ '|'
          : [ if S.member (x, y) grid then '#' else ' '
            | x <- [0 .. mx - 1]
            ]
          ++ ['|']
      | y <- [0 .. my - 1]
      ]
    ++ [(('+' :) $ take mx $ repeat '-') ++ "+"]

-- solve room robots =
--   go room robots 0 S.empty

-- go room robots t seen =
--   if S.member out seen
--     then t
--     else go room robots (t + 1) (S.insert out seen)
--   where
--     out = map (simulate room t) robots
