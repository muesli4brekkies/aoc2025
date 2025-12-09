module D8C2 where

import Data.List (sortBy)
import qualified Data.Set as S

type Node = (Int, Int, Int)
type Wire = (Int, [Node])

splitcirc :: [Int] -> String -> [Int]
splitcirc res "" = res
splitcirc res s = splitcirc (res ++ [read n]) $ drop 1 rst
  where
    (n, rst) = break (== ',') s

shortest :: Wire -> Wire -> Ordering
shortest a b = let first (f, _) = f in first a `compare` first b

countto1000 :: S.Set Node -> [Wire] -> [Node]
countto1000 set ((_, [x, y]) : xs)
  | length set == 1000 = [x, y]
  | otherwise = countto1000 (S.insert x (S.insert y set)) xs
countto1000 _ _ = error ":("

solve :: String -> String
solve input =
  show
    . product
    . map (\c -> let (n,_,_) = c in n)
    . countto1000 S.empty
    $ sortBy shortest wires
  where
    trithagoras :: Node -> Node -> Int
    trithagoras (x, y, z) (a, b, c) = sum $ map (^ 2) [x - a, y - b, z - c]
    totup [x, y, z] = (x, y, z)
    circs = map (totup . splitcirc []) $ lines input
    wires = [(trithagoras a b, [a, b]) | a <- circs, b <- circs, a /= b] :: [Wire]