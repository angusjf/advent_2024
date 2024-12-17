import Data.Bits (xor, (.|.))
import Debug.Trace (trace)

-- main = readFile "test17.txt" >>= print . solve . parse

main = readFile "input17.txt" >>= print . solve . parse

parse :: String -> ((Int, Int, Int), [Int])
parse input = ((read a, read b, read c), map read $ words $ map (\c -> if c == ',' then ' ' else c) program)
  where
    [a, b, c, _, program] = map (drop 1 . snd . break (== ':')) $ lines input

combo (a, b, c) operand =
  case operand of
    4 -> a
    5 -> b
    6 -> c
    7 -> error "combo 7 is reserved"
    n -> n

solve :: ((Int, Int, Int), [Int]) -> [Int]
solve (reg, ins) = run reg ins 0

run :: (Int, Int, Int) -> [Int] -> Int -> [Int]
run reg@(a, b, c) prog ins =
  case drop ins prog of
    [] -> []
    (opcode : operand : _) ->
      case opcode of
        0 -> run (a `div` (2 ^ combo reg operand), b, c) prog (ins + 2)
        1 -> run (a, b `xor` operand, c) prog (ins + 2)
        2 -> run (a, (combo reg operand) `mod` 8, c) prog (ins + 2)
        3 -> case a of
          0 -> run reg prog (ins + 2)
          _ -> run reg prog operand
        4 -> run (a, b `xor` c, c) prog (ins + 2)
        5 -> ((combo reg operand) `mod` 8) : run reg prog (ins + 2)
        6 -> run (a, a `div` (2 ^ combo reg operand), c) prog (ins + 2)
        7 -> run (a, b, a `div` (2 ^ combo reg operand)) prog (ins + 2)
