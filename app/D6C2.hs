module D6C2 where

import Debug.Trace
import Data.List (transpose)

trim :: String -> String
trim = filter (\c -> c /= '0' && c /= ' ')

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n list = first : chunks n rest
  where
    (first, rest) = splitAt n list

calculificate :: Int -> ([Int -> Int -> Int], [[Int]]) -> Int
calculificate res (op : rop, nums : rnum) 
    | null rop || null rnum = res
    | otherwise = trace ((show nums) ++ (show $ foldl1 op nums)) calculificate (res + foldl1 op nums) (rop, rnum)
calculificate _ _ = error ":("

extractOps :: [String] -> ([Int -> Int -> Int], [[Int]])
extractOps l = (map (\o -> if o == '+' then (+) else (*)) $ filter (/= ' ') $ last l, map (map read . transpose . words) $ init l)

solve :: String -> String
solve =
  show . calculificate 0 . extractOps . lines
