import Control.Monad.State
import Data.Char (isNumber)
import Data.Function (on)
import Data.List (notElem, nub, permutations)
import Data.Map qualified as M

main = readFile "input21.txt" >>= print . solve . lines

solve = sum . map (\code -> press code * read (filter isNumber code))

press code = fst $ runState (shortestCode (26, code)) M.empty

shortestCode (0, input) = return (length input)
shortestCode (n, input) =
  sum
    <$> mapM
      (fmap minimum . traverse (\c -> memoized shortestCode (n - 1, c)))
      ( case n of
          26 -> getAllCodes numpad (0, 3) input
          _ -> getAllCodes arrowpad (0, 0) input
      )

getAllCodes padpos illegal code = zipWith (arrowstrs `on` padpos) ('A' : code) code
  where
    arrowstrs (x1, y1) (x2, y2) =
      map (++ "A") $
        filter (notElem illegal . scanl move (x1, y1)) $
          nub $
            permutations $
              concatMap
                (uncurry replicate)
                [ (x2 - x1, '>'),
                  (y1 - y2, '^'),
                  (x1 - x2, '<'),
                  (y2 - y1, 'v')
                ]

arrowpad '<' = (0, 1)
arrowpad '>' = (2, 1)
arrowpad 'A' = (2, 0)
arrowpad '^' = (1, 0)
arrowpad 'v' = (1, 1)

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

move (x, y) '>' = (x + 1, y)
move (x, y) '<' = (x - 1, y)
move (x, y) '^' = (x, y - 1)
move (x, y) 'v' = (x, y + 1)

-- https://angusjf.com/haskell-memoization

runMemoized :: (x -> State (M.Map x y) y) -> x -> y
runMemoized f x = evalState (f x) M.empty

memoized :: (Ord x) => (x -> State (M.Map x y) y) -> x -> State (M.Map x y) y
memoized f x = do
  cache <- get
  case M.lookup x cache of
    Just hit -> return hit
    Nothing -> do
      res <- f x
      modify (M.insert x res)
      return res
