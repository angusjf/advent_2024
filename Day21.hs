import Data.Char (isNumber)
import Data.Function (on)
import Data.List (minimumBy, nub, permutations, sort)
import Data.Set qualified as S

-- -- main = readFile "input21.txt" >>= print . solve . lines
main = readFile "test21.txt" >>= print . solve . lines

solve = map press

press code =
  concatMap
    ( \numcode_part_options ->
        minimumBy (compare `on` length) $
          S.map
            ( \numcode_option ->
                concatMap
                  ( \arrowcode_part_option ->
                      minimumBy (compare `on` length) $
                        S.map
                          (\arrowcode_option_part -> concat $ getCode' arrowpad arrowcode_option_part)
                          arrowcode_part_option
                  )
                  (getCodeOptions arrowpad numcode_option)
            )
            numcode_part_options
    )
    (getCodeOptions numberpad code)

getCodeOptions f code = zipWith (\a b -> arrowstr $ diff (f a) (f b)) ('A' : code) code

getCode' f code = zipWith (\a b -> arrowstr' $ diff (f a) (f b)) ('A' : code) code

diff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

arrowstr (dx, dy) = S.map (++ "A") $ S.fromList $ permutations $ concat [rep dx '>', rep (-dy) '^', rep (-dx) '<', rep dy 'v']
  where
    rep n c = take n (repeat c)

arrowstr' (dx, dy) = concat [rep dx '>', rep (-dy) '^', rep (-dx) '<', rep dy 'v', "A"]
  where
    rep n c = take n (repeat c)

arrowpad '<' = (0, 1)
arrowpad '>' = (2, 1)
arrowpad 'A' = (2, 0)
arrowpad '^' = (1, 0)
arrowpad 'v' = (1, 1)

numberpad '0' = (1, 3)
numberpad '1' = (0, 2)
numberpad '2' = (1, 2)
numberpad '3' = (2, 2)
numberpad '4' = (0, 1)
numberpad '5' = (1, 1)
numberpad '6' = (2, 1)
numberpad '7' = (0, 0)
numberpad '8' = (1, 0)
numberpad '9' = (2, 0)
numberpad 'A' = (2, 3)
-- 029A
-- <A^ A> ^^A vvvA

-- <A
-- -> v<<A
-- -> >>^A
--

-- ^ A
--  -> <A
--  -> >A
--
--   >^^A
--
--   vvvA
--
--
--   vA
--   <^A
--   A
--   >A
--   <vA
--   A
--   A
--   >^A
--
--
--
--   <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
