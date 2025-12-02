module D1C2 where

switchchar :: Char -> Char
switchchar 'R' = '0'
switchchar 'L' = '-'
switchchar c = c

checktick :: Int -> Int -> Ordering -> Int
checktick ac newac grad =
    if newac == 0 || ac /= 0 && ac /= newac && (compare ac newac) == grad 
      then 1
      else 0

spin :: Int -> Int -> [String] -> Int
spin _ res [] = res
spin ac res (s:r) =
  spin newac newres r
  where
    diff = read s
    newac = (+) ac $ read s
    newres = sum [res, abs diff `div` 100, checktick (mod ac 100) (mod newac 100) (compare diff 0)]

solve :: String -> String
solve = show . spin 50 0 . words . map switchchar
