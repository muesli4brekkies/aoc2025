module D1C2 where

switchchar :: Char -> Char
switchchar 'R' = '0'
switchchar 'L' = '-'
switchchar c = c

checktick :: Int -> Int -> Int -> Int
checktick ac newac diff =
  (abs diff) `div` 100
    + if modac /= 0
      && modac /= modnewac
      && (compare modac modnewac) == (compare diff 0)
      || modnewac == 0
      then 1
      else 0
  where
    modac = (mod ac 100)
    modnewac = (mod newac 100)

spin :: Int -> Int -> [String] -> Int
spin _ res [] = res
spin ac res stack =
  spin newac newres newstack
  where
    diff = (read $ stack !! 0 :: Int)
    newac = ac + diff
    newres = res + (checktick ac newac diff)
    newstack = (drop 1 stack)

solve :: String -> String
solve = show . spin 50 0 . words . map switchchar
