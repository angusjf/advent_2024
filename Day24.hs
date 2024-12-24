import Data.List (find, intercalate, intersperse, sort)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)

-- main = readFile "test24.txt" >>= putStrLn . draw . parse

main = readFile "input24.txt" >>= putStrLn . draw . parse

-- main = readFile "input24.txt" >>= print . solve . parse

swaps =
  [ ("z39", "twr"),
    ("z10", "ggn"),
    ("z32", "grm"),
    -- ("jcb", "nvn") 1
    ("jcb", "dnw") -- 2
    -- ("jcb", "ndw") -- 3
  ]

-- ggn,grm,rmn,twr,whq,z10,z32,z39

answer = intercalate "," $ sort $ map fst swaps ++ map snd swaps

parse input = (M.fromList $ map parseWire wires, foldr swap (map parseGate gates) swaps)
  where
    (wires, _ : gates) = break null $ lines input

parseWire wire =
  ( id,
    case n of
      '1' -> True
      '0' -> False
  )
  where
    (id, ':' : ' ' : n : []) = break (== ':') wire

parseGate gate = (out, (a, op, b))
  where
    [a, op, b, "->", out] = words gate

ok (wires, gates) = ((== z) . map snd . filter ((== 'z') . head . fst) . M.toList . fst) <$> outputs
  where
    outputs = find (\(_, gates) -> null gates) $ take 100 $ iterate evalAll (wires, gates)
    x = fromBinary . map snd . filter ((== 'x') . head . fst) $ M.toList wires
    y = fromBinary . map snd . filter ((== 'y') . head . fst) $ M.toList wires
    z = toBinary $ x + y

swap (a, b) g =
  M.toList $
    M.insert a (gates M.! b) $
      M.insert b (gates M.! a) $
        gates
  where
    gates = M.fromList g

toBinary 0 = []
toBinary n = (n `mod` 2 == 1) : toBinary (n `div` 2)

evalAll :: (M.Map String Bool, [(String, (String, String, String))]) -> (M.Map String Bool, [(String, (String, String, String))])
evalAll (intialWires, gates) =
  foldl
    ( \(wires, todo) (id, exp) ->
        case eval wires exp of
          Just result -> (M.insert id result wires, todo)
          Nothing -> (wires, (id, exp) : todo)
    )
    (intialWires, [])
    gates

eval :: M.Map String Bool -> (String, String, String) -> Maybe Bool
eval wires (a, op, b) = do
  x <- M.lookup a wires
  y <- M.lookup b wires
  return $ (toFn op) x y

toFn "AND" = (&&)
toFn "OR" = (||)
toFn "XOR" = (/=)

fromBinary = sum . map (2 ^) . map fst . filter snd . zip [0 ..]

draw :: (M.Map String Bool, [(String, (String, String, String))]) -> String
draw (wires, gates) = unlines $ intersperse "" $ sort $ map (\(id, exp) -> id ++ show exp ++ ": " ++ expand wires (M.fromList gates) exp) $ gates

expand wires gates (a, op, b) = "(" ++ fromMaybe a (f a) ++ " " ++ op ++ " " ++ fromMaybe b (f b) ++ ")"
  where
    f x = expand wires gates <$> M.lookup x gates
