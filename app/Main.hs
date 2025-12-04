module Main where

import Control.Monad (join)
import D10C1
import D10C2
import D11C1
import D11C2
import D12C1
import D12C2
import D1C1
import D1C2
import D2C1
import D2C2
import D3C1
import D3C2
import D4C1
import D4C2
import D5C1
import D5C2
import D6C1
import D6C2
import D7C1
import D7C2
import D8C1
import D8C2
import D9C1
import D9C2
import Data.List ((!?))
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

solvers :: [[String -> String]]
solvers =
  [ [D1C1.solve, D1C2.solve],
    [D2C1.solve, D2C2.solve],
    [D3C1.solve, D3C2.solve],
    [D4C1.solve, D4C2.solve],
    [D5C1.solve, D5C2.solve],
    [D6C1.solve, D6C2.solve],
    [D7C1.solve, D7C2.solve],
    [D8C1.solve, D8C2.solve],
    [D9C1.solve, D9C2.solve],
    [D10C1.solve, D10C2.solve],
    [D11C1.solve, D11C2.solve],
    [D12C1.solve, D12C2.solve]
  ]

oops :: (String -> String)
oops = const "that didn't work\nusage: './aoc2025 4 2 < input_4.txt'\n"

args2Solver :: [String] -> (String -> String)
args2Solver [d, c] = fromMaybe oops $ join $ fmap (!? idx c) solvers !? idx d where idx = pred . read
args2Solver _ = oops

main :: IO ()
main = getArgs >>= interact . args2Solver