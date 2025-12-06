module D5C1 where

parseStr :: String -> (Int, Int)
parseStr input = (mn, mx)
  where
    (mns, mxs) = break (== '-') input
    (mn, mx) = (read mns, read $ drop 1 mxs)

countFresh :: ([String], [Int]) -> Int
countFresh (ranges, nums) = length $ filter (\n -> any (fitsRange n) nRanges) nums
  where
    fitsRange n (mn, mx) = n >= mn && n <= mx
    nRanges = map parseStr ranges :: [(Int, Int)]

splitLn :: [String] -> ([String], [Int])
splitLn input = (ranges, nums)
  where
    (ranges, numStrs) = break (== "") input
    nums = map read $ drop 1 numStrs

solve :: String -> String
solve = show . countFresh . splitLn . lines