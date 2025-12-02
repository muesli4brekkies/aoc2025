module D2C1 where

pairs :: Int -> Bool
pairs n = take half str == drop half str
  where 
    str = show n
    half = (length str) `div` 2

squetch :: [Int] -> [[String]] -> [Int]
squetch res [] = res
squetch res ((x:y:_):rst) = 
  squetch (res ++ [n | n <- [(read x)..(read y)], pairs n])
    rst

segment :: Char -> String -> [String] -> String -> [String]
segment _ ac res [] = res ++ [ac]
segment d ac res (c:rst)
  | c /= d = segment d (ac ++ [c]) res rst
  | otherwise = segment d "" (res ++ [ac]) rst

solve :: String -> String
solve = show . sum . squetch [] . map (segment '-' "" []) .  segment ',' "" []
