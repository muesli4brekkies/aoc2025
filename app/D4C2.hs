module D4C2 where

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

adderupper :: ([Int], [Int], [Int], [Int], [Int], [Int], [Int], [Int]) -> Int -> [Int] -> [Int] -> ([Int], Int)
adderupper _ res lac [] = (lac, res)
adderupper (n, e, s, w, nw, ne, se, sw) res lac (c : rst)
  | c == 0 = adderupper dirs' res lac' rst
  | otherwise =
      let adj = sum $ map head' ldirs
          (res', rlac) = if adj < 4 then (succ res, 0 : lac) else (res, lac')
       in adderupper dirs' res' rlac rst
  where
    ldirs = [n, e, s, w, nw, ne, se, sw]
    dirs' = tupleificate $ map (drop 1) ldirs
    lac' = c : lac

multiwrap :: Int -> Int -> [Int] -> Int -> Int
multiwrap _ 0 _ res = res
multiwrap wid _ lst res = multiwrap wid r lst' res'
  where
    (lst',r) = adderupper (duplificate wid lst) 0 [] lst
    res' = res + r

solve :: String -> String
solve input = show $ multiwrap wid 1 ninput 0
  where
    wid = succ $ length $ takeWhile (/= '\n') input
    ninput = map numberificate input