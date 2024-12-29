import Data.Char (isNumber)
import Data.Function (on)
import Data.List (minimumBy, nub, permutations, sort)
import Data.Set qualified as S
import Debug.Trace (trace)

main = readFile "input21.txt" >>= print . solve . lines

-- main = readFile "test21.txt" >>= print . solve . lines

solve = sum . map (\code -> length (press code) * read (filter isNumber code))

longest = minimumBy (compare `on` length)

press =
  foldr
    (\f acc -> concatMap (longest . S.map acc) . f)
    id
    (getCodeN : (take 2 $ repeat getCodeA))

getCodeA x = getCodeOptions arrowpad arrowpadvalid x

getCodeN = getCodeOptions numberpad numberpadvalid

getCodeOptions f valid code = zipWith (\a b -> S.filter (valid (f a) (f b)) $ arrowstr $ diff (f a) (f b)) ('A' : code) code

getCode' f code = zipWith (\a b -> arrowstr' $ diff (f a) (f b)) ('A' : code) code

diff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

arrowstr (dx, dy) = S.map (++ "A") $ S.fromList $ permutations $ concat [rep dx '>', rep (-dy) '^', rep (-dx) '<', rep dy 'v']
  where
    rep n c = take n (repeat c)

applyArrow :: (Int, Int) -> Char -> (Int, Int)
applyArrow (x, y) '>' = (x + 1, y)
applyArrow (x, y) '<' = (x - 1, y)
applyArrow (x, y) '^' = (x, y - 1)
applyArrow (x, y) 'v' = (x, y + 1)

numberpadvalid :: (Int, Int) -> (Int, Int) -> String -> Bool
numberpadvalid from to "A" = True
numberpadvalid from to (x : xs) =
  let new = applyArrow from x
   in new /= (0, 3) && numberpadvalid new to xs

arrowpadvalid :: (Int, Int) -> (Int, Int) -> String -> Bool
arrowpadvalid from to "A" = True
arrowpadvalid from to (x : xs) =
  let new = applyArrow from x
   in new /= (0, 0) && arrowpadvalid new to xs

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
