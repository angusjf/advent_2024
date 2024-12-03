import Data.Char (isDigit)

main = readFile "input03.txt" >>= print . run True 0

run enabled sum "" = sum
run enabled sum input = case mul input of
  Just ((a, b), more) -> run enabled (if enabled then sum + a * b else sum) more
  Nothing -> case enable input of
    Just (enabled, more) -> run enabled sum more
    Nothing -> run enabled sum (drop 1 input)

enable ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : more) = Just (False, more)
enable ('d' : 'o' : '(' : ')' : more) = Just (True, more)
enable _ = Nothing

mul :: String -> Maybe ((Int, Int), String)
mul all@('m' : 'u' : 'l' : '(' : more) =
  case num more of
    Just (a, ',' : more') ->
      case num more' of
        Just (b, ')' : more'') -> Just ((a, b), more'')
        _ -> Nothing
    _ -> Nothing
mul _ = Nothing

num xs =
  case takeWhile isDigit xs of
    [] -> Nothing
    n -> Just (read n, drop (length n) xs)
