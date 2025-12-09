module D6C1 where

import Data.List(transpose)

calculificate :: [String] -> Int
calculificate l = foldl op st $ map read $ init l
    where (op,st) = if last l == "+" then ((+),0); else ((*),1)

solve :: String -> String
solve = show . sum . map calculificate . transpose . map words . lines
