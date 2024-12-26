import Control.Monad (guard)
import Data.List (delete, elem, find, last, nub, singleton)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Tuple (swap)
import Debug.Trace (trace)

-- main = readFile "test23.txt" >>= print . solve . parse

main = readFile "input23.txt" >>= print . solve . parse

parse = map ((\(a, ('-' : xs)) -> (a, xs)) . break (== '-')) . lines

solve xs =
  let nodes = S.fromList $ map fst xs ++ map snd xs
      lookup = (toBiLookup xs)
   in cliques lookup nodes (S.singleton (S.empty))

toBiLookup :: (Ord k) => [(k, k)] -> M.Map k [k]
toBiLookup kvs =
  M.map nub $
    M.unionWith
      (++)
      (M.fromListWith (++) $ map kvl kvs)
      (M.fromListWith (++) $ map (kvl . swap) kvs)
  where
    kvl (k, v) = (k, [v])

cliques lookup nodes cs =
  let cs' =
        S.unions $
          S.map
            ( \c ->
                S.map
                  (\n -> (S.insert n c))
                  $ S.filter (\n -> all (\c -> c `elem` lookup M.! n) c)
                  $ nodes
            )
            cs
   in if S.null cs'
        then cs
        else cliques lookup nodes $ trace (show $ S.size cs') cs'
