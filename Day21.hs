import Control.Monad
import Control.Monad.State
import Data.Char (isNumber)
import Data.Foldable
import Data.Function (on)
import Data.List (minimumBy, nub, permutations, sort)
import Data.Map qualified as Map
import Data.Set qualified as S
import Debug.Trace (trace)

main = readFile "input21.txt" >>= print . solve . lines

solve = sum . map (\code -> length (press code) * read (filter isNumber code))

longest :: S.Set String -> String
longest = minimumBy (compare `on` length)

press :: String -> String
press code = fst $ runState (press' code) Map.empty

smap :: (String -> State (Map.Map String [S.Set String]) String) -> S.Set String -> State (Map.Map String [S.Set String]) (S.Set String)
smap f xs = S.fromList <$> mmap f (S.toList xs)

mmap :: (String -> State (Map.Map String [S.Set String]) String) -> [String] -> State (Map.Map String [S.Set String]) [String]
mmap f [] = return []
mmap f (x : xs) =
  do
    x' <- f x
    (x' :) <$> mmap f xs

mconcatMap :: (S.Set String -> State (Map.Map String [S.Set String]) String) -> [S.Set String] -> State (Map.Map String [S.Set String]) String
mconcatMap f xs = concat <$> mapM f xs

ggg [] = return
ggg (f : fs) =
  ( \input1 ->
      f input1
        >>= ( mconcatMap
                ( \w ->
                    longest
                      <$> smap
                        (ggg fs)
                        w
                )
            )
  )

press' :: String -> State (Map.Map String [S.Set String]) String
press' code =
  ggg
    (getCodeN : (take 2 $ repeat getCodeA))
    code

press_ code =
  let aa = \input -> concatMap (longest . S.map id) $ getCodeA_old input
      bb = \input -> concatMap (longest . S.map aa) $ getCodeA_old input
      cc = \input -> concatMap (longest . S.map bb) $ getCodeN_old input
   in cc code

getCodeA :: String -> State (Map.Map String [S.Set String]) [S.Set String]
getCodeA x =
  do
    cache <- get
    case Map.lookup x cache of
      Just hit -> return hit
      Nothing -> do
        let res = getCodeOptions arrowpad arrowpadvalid x
        modify (Map.insert x res)
        return res

getCodeN :: String -> State (Map.Map String [S.Set String]) [S.Set String]
getCodeN x = return $ getCodeOptions numberpad numberpadvalid x

getCodeA_old :: String -> [S.Set String]
getCodeA_old x = getCodeOptions arrowpad arrowpadvalid x

getCodeN_old :: String -> [S.Set String]
getCodeN_old x = getCodeOptions numberpad numberpadvalid x

getCodeOptions f valid code = zipWith (\a b -> S.filter (valid (f a) (f b)) $ arrowstr $ diff (f a) (f b)) ('A' : code) code

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
