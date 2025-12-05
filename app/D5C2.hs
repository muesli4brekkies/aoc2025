module D5C2 where

parseStr :: String -> Int
parseStr input = 2 + (mx - mn)
  where
    (mns, mxs) = break (== '-') input
    (mn, mx) = (read mns, read $ drop 1 mxs)

solve :: String -> String
solve input = show $ map parseStr ranges
  where
    ranges = takeWhile (/= "") $ lines input
