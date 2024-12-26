import Data.Char (isDigit)
import Data.List (sort)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Ratio (denominator, numerator)

-- main = readFile "test13.txt" >>= print . solve . parse

main = readFile "input13.txt" >>= print . solve . parse

parse = map parseBlock . blankLines . lines

blankLines :: [String] -> [[String]]
blankLines input =
  case break null input of
    (x, []) -> [x]
    (x, _ : xs) -> x : blankLines xs

parseBlock :: [String] -> ((Int, Int), (Int, Int), (Int, Int))
parseBlock [a, b, prize] =
  let (ax, ay) = numbers a
      (bx, by) = numbers b
      (prizex, prizey) = numbers prize
   in ((ax, ay), (bx, by), (prizex, prizey))
  where
    numbers s =
      let (p1, p2) = break (== ',') s
       in (read $ filter isDigit p1, read $ filter isDigit p2)

solve =
  sum
    . mapMaybe fewestTokens
    . map (\(a, b, (prizex, prizey)) -> (a, b, (prizex + 10000000000000, prizey + 10000000000000)))

fff :: ((Int, Int), (Int, Int), (Int, Int)) -> ((Rational, Rational), (Rational, Rational), (Rational, Rational))
fff ((ax, ay), (bx, by), (prizex, prizey)) = ((toRational ax, toRational ay), (toRational bx, toRational by), (toRational prizex, toRational prizey))

fewestTokens block =
  do
    let block' = fff block
    a <- rationalToInt $ getA block'
    b <- rationalToInt $ getB a block'
    pure (3 * a + b)

rationalToInt r
  | denominator r == 1 = Just (numerator r)
  | otherwise = Nothing

getA :: ((Rational, Rational), (Rational, Rational), (Rational, Rational)) -> Rational
getA ((ax, ay), (bx, by), (prizex, prizey)) =
  (prizex - (prizey * (bx / by))) / (ax - (ay * bx / by))

getB :: Integer -> ((Rational, Rational), (Rational, Rational), (Rational, Rational)) -> Rational
getB a ((ax, _), (bx, _), (prizex, _)) =
  (prizex - (toRational a) * ax) / bx
