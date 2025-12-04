module Main where

-- import qualified D4C1
-- import qualified D4C2
-- import qualified D5C1
-- import qualified D5C2
-- import qualified D6C1
-- import qualified D6C2
-- import qualified D7C1
-- import qualified D7C2
-- import qualified D8C1
-- import qualified D8C2
-- import qualified D9C1
-- import qualified D9C2
-- import qualified D10C1
-- import qualified D10C2
-- import qualified D11C1
-- import qualified D11C2
-- import qualified D12C1
-- import qualified D12C2

import Control.Monad (when)
import qualified D1C1
import qualified D1C2
import qualified D2C1
import qualified D2C2
import qualified D3C1
import qualified D3C2
import System.Environment as Sys

solutions :: [[String -> String]]
solutions =
  [ [ D1C1.solve,
      D1C2.solve
    ],
    [ D2C1.solve,
      D2C2.solve
    ],
    [ D3C1.solve,
      D3C2.solve
    ] {-,
      [ D3C1.solve,
        D3C2.solve
      ],
      [ D4C1.solve,
        D4C2.solve
      ],
      [ D5C1.solve,
        D5C2.solve
      ],
      [ D6C1.solve,
        D6C2.solve
      ],
      [ D7C1.solve,
        D7C2.solve
      ],
      [ D8C1.solve,
        D8C2.solve
      ],
      [ D9C1.solve,
        D9C2.solve
      ],
      [ D10C1.solve,
        D10C2.solve
      ],
      [ D11C1.solve,
        D11C2.solve
      ],
      [ D12C1.solve,
        D12C2.solve
      ]-}
  ]

files :: [String]
files =
  [ "inputs/1.in",
    "inputs/2.in",
    "inputs/3.in"
  ]

printywinty :: (String -> String) -> String -> IO ()
printywinty solver input = print $ solver input

main :: IO ()
main =
  Sys.getArgs >>= \args ->
    if length args /= 2
      then print "need day and challenge to run, i.e aoc2025 4 1"
      else do
        let (d, c) = (pred $ read (head args), pred $ read (args !! 1))
        readFile (files !! d) >>= printywinty (solutions !! d !! c)
