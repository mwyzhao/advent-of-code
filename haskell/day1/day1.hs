import Data.List (sort)

main = do
  contents <- getContents
  let
    nums = map ((map (read :: String -> Int)) . words) (lines contents)
    xs2 = map head nums
    ys2 = map last nums
    xs1 = sort xs2
    ys1 = sort ys2
    p1_ans = p1 xs1 ys1
    p2_ans = p2 xs2 ys2
  print p1_ans
  print p2_ans

p1 :: [Int] -> [Int] -> Int
p1 xs ys = sum (map (\(x,y) -> abs (x-y)) (zip xs ys))

p2 :: [Int] -> [Int] -> Int
p2 xs ys = sum (map getScore xs)
  where
    getScore :: Int -> Int
    getScore x = x * (length (filter (\y -> y == x) ys))
