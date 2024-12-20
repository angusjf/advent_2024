import Data.Bits (shift, xor)

-- main = readFile "test17.txt" >>= print . solve . parse

main = readFile "input17.txt" >>= print . solve . parse

parse :: String -> ((Int, Int, Int), [Int])
parse input = ((read a, read b, read c), map read $ words $ map (\c -> if c == ',' then ' ' else c) program)
  where
    [a, b, c, _, program] = map (drop 1 . snd . break (== ':')) $ lines input

tooHigh = 178388203060038

toooLow = 140737488355328

orWyArd = 214237055846652

desired = [2, 4, 1, 1, 7, 5, 1, 5, 0, 3, 4, 3, 5, 5, 3, 0]

solve _ = output tooHigh

n =
  let a0 = 6
      a1 = 0
      a2 = 5
      a3 = 5
      a4 = 3
      a5 = 11
      a6 = 5
      a7 = 11
      a8 = 4
      a9 = 7
      a10 = 0
      a11 = 7
      a12 = 11
      a13 = 11
      a14 = 7
      a15 = 4

      ap n x = (8 ^ n) * x
   in sum
        [ ap 0 a0,
          ap 1 a1,
          ap 2 a2,
          ap 3 a3,
          ap 4 a4,
          ap 5 a5,
          ap 6 a6,
          ap 7 a7,
          ap 8 a8,
          ap 9 a9,
          ap 10 a10,
          ap 11 a11,
          ap 12 a12,
          ap 13 a13,
          ap 14 a14,
          ap 15 a15
        ]

output a | a `div` 8 == 0 = f a : []
output a = f a : output (a `div` 8)

f a = a `mod` 8 `xor` 1 `xor` 5 `xor` (a `div` (1 `shift` (a `mod` 8 `xor` 1))) `mod` 8
