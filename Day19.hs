import Control.Monad.State
import Data.List (find)
import Data.Map qualified as M
import Debug.Trace (trace)

main = readFile "test19.txt" >>= print . solve . parse

-- main = readFile "input19.txt" >>= print . solve . parse

memoized :: (Ord x) => (x -> State (M.Map x y) y) -> x -> State (M.Map x y) y
memoized f x = do
  cache <- get -- get the 'cache' from the state monad
  case M.lookup x cache of -- check if the key `x` exists in the cache
    Just hit -> return hit -- if it exists, return it
    Nothing -> do
      res <- f x -- else, call f(x)
      modify (M.insert x res) -- set cache[x] = f(x)
      return res -- return f(x)

runMemoized :: (x -> State (M.Map x y) y) -> x -> y
runMemoized f x = evalState (f x) M.empty

parse input = (words $ filter (/= ',') patterns, designs)
  where
    (patterns : _ : designs) = lines input

solve (patterns, designs) = sum $ map (\design -> runMemoized ways (patterns, design)) designs

ways :: ([String], String) -> State (M.Map ([String], String) Int) Int
ways (_, []) = return 1
ways (patterns, design) =
  sum
    <$> mapM
      (\s -> memoized ways (patterns, drop (length s) design))
      (filter (startsWith design) patterns)

startsWith long sub = take (length sub) long == sub
