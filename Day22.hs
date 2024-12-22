import Data.Bits (xor)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S

main = readFile "input22.txt" >>= print . solve . parse

parse :: String -> [Int]
parse = map read . lines

evolve :: Int -> Int
evolve = amp (* 2048) . amp (`div` 32) . amp (* 64)

amp f n = (n `xor` f n) `mod` 16777216

changes :: (Num a) => [a] -> [a]
changes xs = zipWith (-) (drop 1 xs) xs

---------------------------------------------

solve input =
  maximum $
    M.elems $
      M.unionsWith (+) $
        map (M.fromListWith (flip const) . patternPrices . add changes . map (`mod` 10) . take 2001 . iterate evolve) input

add f xs = zip (f xs) xs

patternPrices xs@((a, _) : (b, _) : (c, _) : (d, _) : (_, n) : _) = ((a, b, c, d), n) : patternPrices (tail xs)
patternPrices _ = []
