module D5C2 where

import Data.Maybe (catMaybes, fromJust)

mHead' :: [(Int, Int)] -> Maybe (Int, Int)
mHead' [] = Nothing
mHead' l = Just (head l)

collapseRanges :: Maybe (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
collapseRanges Nothing _ = Nothing
collapseRanges jtx ty
  | minx >= miny && maxx <= maxy = Nothing
  | minx >= miny && fitsRange (minx, maxx) maxy = Just (miny, maxx)
  | maxx <= maxy && fitsRange (minx, maxx) miny = Just (minx, maxy)
  | otherwise = jtx
  where
    fitsRange (mn, mx) n = n >= mn && n <= mx
    tx = fromJust jtx
    ((minx, maxx), (miny, maxy)) = (tx, ty)

loopadoop :: [(Int, Int)] -> Int -> [(Int, Int)]
loopadoop res 0 = res
loopadoop res n = loopadoop res' $ pred n
  where
    (rf, (r : rb)) = splitAt n res
    res' = rf ++ catMaybes [foldl collapseRanges (Just r) (rf ++ rb)] ++ rb

doopwrap :: [(Int, Int)] -> Int -> [(Int, Int)]
doopwrap lst len
  | len /= len' = doopwrap lst' len'
  | otherwise = lst
  where
    lst' = loopadoop lst $ pred $ length lst
    len' = length lst'

parseRange :: String -> (Int, Int)
parseRange input = let (mns, mxs) = break (== '-') input in (read mns, read $ drop 1 mxs)

diffTuples :: (Int, Int) -> Int
diffTuples (a, b) = succ b - a

solve :: String -> String
solve input = show $ sum $ map diffTuples $ doopwrap pRanges (length pRanges)
  where
    ranges = takeWhile (/= "") $ lines input
    pRanges = map parseRange ranges
