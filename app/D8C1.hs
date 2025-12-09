module D8C1 where

import Data.List (sortBy)
import Data.Ord (Down (Down), comparing)
import qualified Data.Set as S

type Node = (Int, Int, Int)

type Wire = (Int, [Node])

unionise :: S.Set Node -> S.Set Node -> S.Set Node
unionise s t
  | S.disjoint s t = t
  | otherwise = S.union s t

solder :: [S.Set Node] -> [S.Set Node] -> [S.Set Node]
solder res [] = res
solder res (ab : xs) = solder res' xs
  where
    res'
      | all (S.disjoint ab) res = ab : res
      | otherwise = map (unionise ab) res

solderwrap :: [S.Set Node] -> [S.Set Node]
solderwrap res
  | length res == length res' = res
  | otherwise = solderwrap res'
  where
    res' = solder [] res

splitcirc :: [Int] -> String -> [Int]
splitcirc res "" = res
splitcirc res s = splitcirc (res ++ [read n]) $ drop 1 rst
  where
    (n, rst) = break (== ',') s

dropother :: [[Node]] -> [[Node]] -> [[Node]]
dropother res [] = res
dropother res (x : xs) = dropother (res ++ [x]) $ drop 1 xs

shortest :: Wire -> Wire -> Ordering
shortest a b = let first (f, _) = f in first a `compare` first b

solve :: String -> String
solve input =
  show
    . product
    . take 3
    . sortBy (comparing Data.Ord.Down)
    . map length
    . solderwrap
    . map S.fromList
    . take 1000
    . dropother []
    . map (\c -> let (_, a) = c in a)
    $ sortBy shortest wires
  where
    trithagoras :: Node -> Node -> Int
    trithagoras (x, y, z) (a, b, c) = sum $ map (^ 2) [x - a, y - b, z - c]
    totup [x, y, z] = (x, y, z)
    circs = map (totup . splitcirc []) $ lines input
    wires = [(trithagoras a b, [a, b]) | a <- circs, b <- circs, a /= b] :: [Wire]