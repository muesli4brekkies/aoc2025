module D1C1 where

solve :: String -> String
solve = show . sum . map (\n -> if (n `mod` 100) == 0 then 1; else 0) . scanl (+) 50 . map (read::String->Int) . words . map (\c -> if c == 'L' then '-'; else if c == 'R' then '0'; else c)
