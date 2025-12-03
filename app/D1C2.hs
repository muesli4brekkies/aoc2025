module D1C2 where

switchchar :: Char -> Char
switchchar 'R' = '0'
switchchar 'L' = '-'
switchchar c = c

ytick :: Bool -> Int
ytick True = 1
ytick False = 0

checktick :: Int -> Int -> Int
checktick ac diff = ytick (y == 0 || x /= 0 && compare x y == grad)
  where
    x = mod ac 100
    y = mod (ac + diff) 100
    grad = compare diff 0

spin :: Int -> Int -> [Int] -> Int
spin _ res [] = res
spin ac res (n : r) = spin (ac + n) (sum [res, abs n `div` 100, checktick ac n]) r

solve :: String -> String
solve = show . spin 50 0 . map read . words . map switchchar