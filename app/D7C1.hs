module D7C1 where

import Data.List ((!?))
import Data.Maybe (fromMaybe)

abovef :: Char -> Char
abovef '|' = '|'
abovef 'S' = '|'
abovef _ = '.'

splitificate :: Int -> Int -> String -> String -> Int
splitificate _ nres _ "" = nres
splitificate wid nres res (x : xs)
  | x == '.' = splitificate wid nres (res ++ [above]) xs
  | otherwise =
      if above == '|' 
        then splitificate wid (succ nres) (init res ++ '|' : x : "|") (drop 1 xs)
        else splitificate wid nres (res ++ [x]) xs
  where
    above = abovef $ fromMaybe '.' $ res !? (length res - wid)

solve :: String -> String
solve input = show $ splitificate wid 0 "" $ filter (/= '\n') input
  where
    wid = length $ takeWhile (/= '\n') input
