import Control.Monad.State
import Data.Char (isNumber)
import Data.List (permutations)
import Data.Map qualified as Map

main = readFile "input21.txt" >>= print . solve . lines

solve = sum . map (\code -> press code * read (filter isNumber code))

press :: String -> Int
press code = fst $ runState (bestcode 26 code) Map.empty

functions = (take 25 $ repeat getCodeA) ++ [getCodeN]

bestcode :: Int -> String -> State (Map.Map (String, Int) Int) Int
bestcode 0 = return . length
bestcode n =
  \input ->
    do
      cache <- get
      case Map.lookup (input, n) cache of
        Just hit -> return hit
        Nothing ->
          do
            let all_options = (functions !! (n - 1)) input
            res <- sum <$> mapM (\w -> minimum <$> traverse (bestcode (n - 1)) w) all_options
            modify (Map.insert (input, n) res)
            return res

getCodeA :: String -> [[String]]
getCodeA = getCodeOptions arrowpad arrowpadvalid

getCodeN :: String -> [[String]]
getCodeN = getCodeOptions numberpad numberpadvalid

getCodeOptions f valid code = zipWith (\a b -> filter (valid (f a) (f b)) $ arrowstr $ diff (f a) (f b)) ('A' : code) code

diff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

arrowstr (dx, dy) = map (++ "A") $ permutations $ concat [rep dx '>', rep (-dy) '^', rep (-dx) '<', rep dy 'v']
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
