{-# LANGUAGE TupleSections #-}
module D7C2 where

import Data.List ((!?))
import Data.Maybe (fromMaybe)

abovef :: (Int, Char) -> (Int, Char)
abovef (n, '|') = (n, '|')
abovef (_, 'S') = (1, '|')
abovef _ = (0, '.')

getabove :: [(Int, Char)] -> Int -> (Int, Char)
getabove res dif = abovef $ fromMaybe (0, '.') $ res !? (length res - dif)

splitificate :: Int -> Int -> [(Int, Char)] -> [(Int, Char)] -> [(Int, Char)]
splitificate _ _ res [] = res
splitificate wid nres res (x : xs)
  | snd x == '.' = splitificate wid nres (res ++ [above]) xs
  | otherwise =
      if snd above == '|'
        then splitificate wid (succ nres) (init res ++ (fst above + fst (last res), '|') : x : [(fst above + fst abover, '|')]) (drop 1 xs)
        else splitificate wid nres (res ++ [x]) xs
  where
    above = getabove res wid
    abover = getabove res (succ wid)

solve :: String -> String
solve input =
  show $
    sum $
      map fst
      -- \$ map snd
      $
        drop (length ninput - wid) $
          splitificate wid 0 [] ninput
  where
    wid = length $ takeWhile (/= '\n') input
    ninput = map (0,) $ filter (/= '\n') input
