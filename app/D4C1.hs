module D4C1 where

head' :: [Int] -> Int
head' [] = 0
head' l = head l

numberificate :: Char -> Int
numberificate '@' = 1
numberificate _ = 0

duplificate :: Int -> [Int] -> ([Int], [Int], [Int], [Int], [Int], [Int], [Int], [Int])
duplificate wid s = (buf ++ s, drop 1 s, drop wid s, 0 : s, buf ++ [0] ++ s, drop 1 buf ++ s, drop (succ wid) s, drop (pred wid) s)
  where
    buf = replicate wid 0

shortenList :: [Int] -> [Int]
shortenList (_ : rst) = rst
shortenList _ = []

tupleificate :: [[Int]] -> ([Int], [Int], [Int], [Int], [Int], [Int], [Int], [Int])
tupleificate [n, e, s, w, nw, ne, se, sw] = (n, e, s, w, nw, ne, se, sw)
tupleificate _ = error ":("

adderupper :: ([Int], [Int], [Int], [Int], [Int], [Int], [Int], [Int]) -> Int -> [Int] -> Int
adderupper _ res [] = res
adderupper (n, e, s, w, nw, ne, se, sw) res (c : rst)
  | c == 0 = adderupper dirs' res rst
  | otherwise =
      let adj = sum $ map head' ldirs
          res' = if adj < 4 then succ res else res
       in adderupper dirs' res' rst
  where
    ldirs = [n, e, s, w, nw, ne, se, sw]
    dirs' = tupleificate $ map (drop 1) ldirs

solve :: String -> String
solve input = show $ adderupper (duplificate wid ninput) 0 ninput
  where
    wid = succ $ length $ takeWhile (/= '\n') input
    ninput = map numberificate input