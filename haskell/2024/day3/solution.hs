import Data.Char (isDigit)

main = do
  contents <- getContents
  let p1_ans = p1 contents
      p2_ans = p2 contents
  print p1_ans
  print p2_ans

parseArg' :: String -> String -> String
parseArg' (c : s) n = if isDigit c then parseArg' s (c : n) else n

parseArg :: String -> Maybe String
parseArg s@(c : _) = if isDigit c then Just (reverse (parseArg' s [])) else Nothing

data ParseState = StartState | LeftArgState | DelimState | RightArgState | EndState

parse :: String -> [Integer] -> ParseState -> Integer -> [Integer]
parse [] xs _ _ = xs
parse ('m' : 'u' : 'l' : '(' : s) xs StartState _ = parse s xs LeftArgState 0
parse s xs StartState _ = parse (tail s) xs StartState 0
parse s xs LeftArgState _ =
  let arg = parseArg s
   in case arg of
        Just a -> parse (drop (length a) s) xs DelimState (read a)
        Nothing -> parse (tail s) xs StartState 0
parse (c : s) xs DelimState left = if c == ',' then parse s xs RightArgState left else parse s xs StartState 0
parse s xs RightArgState left =
  let arg = parseArg s
   in case arg of
        Just a -> parse (drop (length a) s) xs EndState ((read a) * left)
        Nothing -> parse (tail s) xs StartState 0
parse (c : s) xs EndState val = if c == ')' then parse s (val : xs) StartState 0 else parse s xs StartState 0

p1 :: String -> Integer
p1 s = sum (parse s [] StartState 0)

p2 :: String -> Integer
p2 s = sum (parse2 s [] DoState)

parseMul' :: String -> ParseState -> Integer -> Int -> Maybe (Integer, Int)
parseMul' [] _ _ _ = Nothing
parseMul' ('m' : 'u' : 'l' : '(' : s) StartState _ _ = parseMul' s LeftArgState 0 4
parseMul' _ StartState _ _ = Nothing
parseMul' s LeftArgState _ num =
  let arg = parseArg s
   in case arg of
        Just a -> let added_num = length a in parseMul' (drop added_num s) DelimState (read a) (num + added_num)
        Nothing -> Nothing
parseMul' (c : s) DelimState left num = if c == ',' then parseMul' s RightArgState left (num + 1) else Nothing
parseMul' s RightArgState left num =
  let arg = parseArg s
   in case arg of
        Just a -> let added_num = length a in parseMul' (drop added_num s) EndState ((read a) * left) (num + added_num)
        Nothing -> Nothing
parseMul' (c : s) EndState val num = if c == ')' then Just (val, (num + 1)) else Nothing

parseMul :: String -> (Bool, Integer, Int)
parseMul s =
  let res = parseMul' s StartState 0 0
   in case res of
        Just (val, num) -> (True, val, num)
        Nothing -> (False, 0, 0)

data EnableState = DoState | DontState

parseEnableState :: String -> Maybe EnableState
parseEnableState ('d' : 'o' : '(' : ')' : s) = Just DoState
parseEnableState ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : s) = Just DontState
parseEnableState _ = Nothing

parse2 :: String -> [Integer] -> EnableState -> [Integer]
parse2 [] xs _ = xs
parse2 s xs DoState =
  let state = parseEnableState s
   in case state of
        Just DoState -> parse2 (drop 4 s) xs DoState
        Just DontState -> parse2 (drop 7 s) xs DontState
        Nothing ->
          let (success, val, num) = parseMul s
           in if success then parse2 (drop num s) (val : xs) DoState else parse2 (tail s) xs DoState
parse2 s xs DontState =
  let state = parseEnableState s
   in case state of
        Just DoState -> parse2 (drop 4 s) xs DoState
        Just DontState -> parse2 (drop 7 s) xs DontState
        Nothing -> parse2 (tail s) xs DontState
