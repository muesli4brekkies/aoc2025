module D2C2 where

match :: String -> Bool
match str = or [concat (replicate (lim `div` x) (take x str)) == str | x <- [1..lim `div` 2], (lim `div` x) * x == lim]
  where lim = length str

squetch :: [Int] -> [[String]] -> [Int]
squetch res [] = res
squetch res ((x:y:_):rst) = squetch (res ++ [n | n <- [(read x)..(read y)], match (show n)]) rst
squetch _ _ = error "woops"

segment :: Char -> [String] -> String -> [String]
segment _ res "" = res 
segment c res str = segment c (res ++ [snip]) (drop (succ $ length snip) str) where snip = takeWhile (/= c) str

solve :: String -> String
solve = show . sum . squetch [] . map (segment '-' []) . segment ',' []
