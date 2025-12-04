module D3C1 where

findMax :: Int -> String -> String -> Int
findMax 0 res _ = read res
findMax expn res line = findMax expn' res' line'
  where
    expn' = pred expn
    mx = maximum $ take (length line - expn') line
    res' = res ++ [mx]
    line' = drop 1 $ dropWhile (/= mx) line

solve :: String -> String
solve = show . sum . map (findMax 2 "") . lines