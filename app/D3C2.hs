module D3C2 where

findMax :: Int -> Int -> String -> Int
findMax 0 res _ = res
findMax expn res line = findMax expn' res' line'
  where
    expn' = pred expn
    mx = maximum $ take (length line - expn') line
    res' = 10 * res + read [mx]
    line' = drop 1 $ dropWhile (/= mx) line

solve :: String -> String
solve = show . sum . map (findMax 12 0) . lines