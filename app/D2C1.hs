module D2C1 where

match :: String -> Bool
match str = take lim str == drop lim str
  where
    lim = length str `div` 2

squetch :: [Int] -> [[String]] -> [Int]
squetch res [] = res
squetch res ((x : y : _) : rst) = squetch (res ++ [n | n <- [(read x) .. (read y)], match (show n)]) rst
squetch _ _ = error "woops"

segment :: Char -> [String] -> String -> [String]
segment _ res "" = res
segment c res str = segment c res' str'
  where
    (fore, aft) = break (== c) str
    res' = res ++ [fore]
    str' = drop 1 aft

solve :: String -> String
solve = show . sum . squetch [] . map (segment '-' []) . segment ',' []
