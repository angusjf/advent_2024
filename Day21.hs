import Control.Monad.State
import Data.Char (isNumber)
import Data.List (notElem, permutations)
import Data.Map qualified as Map

main = readFile "input21.txt" >>= print . solve . lines

solve = sum . map (\code -> press code * read (filter isNumber code))

press :: String -> Int
press code = fst $ runState (bestcode 26 code) Map.empty

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
            res <-
              sum
                <$> mapM
                  (\w -> minimum <$> traverse (bestcode (n - 1)) w)
                  (getCode (n - 1) input)
            modify (Map.insert (input, n) res)
            return res

getCode 25 = getCodeN
getCode _ = getCodeA

getCodeA :: String -> [[String]]
getCodeA = getCodeOptions arrowpad (0, 0)
  where
    arrowpad '<' = (0, 1)
    arrowpad '>' = (2, 1)
    arrowpad 'A' = (2, 0)
    arrowpad '^' = (1, 0)
    arrowpad 'v' = (1, 1)

getCodeN :: String -> [[String]]
getCodeN = getCodeOptions numpad (0, 3)
  where
    numpad '0' = (1, 3)
    numpad '1' = (0, 2)
    numpad '2' = (1, 2)
    numpad '3' = (2, 2)
    numpad '4' = (0, 1)
    numpad '5' = (1, 1)
    numpad '6' = (2, 1)
    numpad '7' = (0, 0)
    numpad '8' = (1, 0)
    numpad '9' = (2, 0)
    numpad 'A' = (2, 3)

getCodeOptions f illegal code =
  zipWith
    (\a b -> filter (notElem illegal . positions (f a) (f b)) $ arrowstr $ diff (f a) (f b))
    ('A' : code)
    code
  where
    positions from to "A" = []
    positions from to (x : xs) = let new = applyArrow from x in new : positions new to xs
    diff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)
    arrowstr (dx, dy) = map (++ "A") $ permutations $ concat [replicate dx '>', replicate (-dy) '^', replicate (-dx) '<', replicate dy 'v']

    applyArrow :: (Int, Int) -> Char -> (Int, Int)
    applyArrow (x, y) '>' = (x + 1, y)
    applyArrow (x, y) '<' = (x - 1, y)
    applyArrow (x, y) '^' = (x, y - 1)
    applyArrow (x, y) 'v' = (x, y + 1)
