main = do
  contents <- getContents
  let nums = map ((map (read :: String -> Int)) . words) (lines contents)
      p1_ans = p1 nums
      p2_ans = p2 nums
  print p1_ans
  print p2_ans

p1 :: [[Int]] -> Int
p1 xs = length (filter safe xs)
  where
    safe :: [Int] -> Bool
    safe [] = True
    safe (x : xs) = safe' x xs pred
      where
        pred :: Int -> Int -> Bool
        pred = if ((head xs) > x) then (>) else (<)
        safe' :: Int -> [Int] -> (Int -> Int -> Bool) -> Bool
        safe' _ [] _ = True
        safe' prev_x (x : xs) pred = (pred x prev_x) && ((abs (x - prev_x)) < 4) && (safe' x xs pred)

p2 :: [[Int]] -> Int
p2 xs = length (filter safe xs)
  where
    safe :: [Int] -> Bool
    safe [] = True
    safe (x : xs) = (safe' False x xs (>)) || (safe' False x xs (<))
      where
        cond :: Int -> Int -> (Int -> Int -> Bool) -> Bool
        cond x prev_x pred = (pred x prev_x) && ((abs (x - prev_x)) < 4)
        safe' :: Bool -> Int -> [Int] -> (Int -> Int -> Bool) -> Bool
        safe' _ _ [] _ = True
        safe' False _ [x] _ = True
        safe' False prev_x (x : xs) pred = if (cond x prev_x pred) then (safe' False x xs pred) else (safe' True x (tail xs) pred) || (safe' True (head xs) (tail xs) pred)
        safe' True prev_x (x : xs) pred = (cond x prev_x pred) && (safe' True x xs pred)
