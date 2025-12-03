module Main where

import qualified D1C1
import qualified D1C2
import qualified D2C1
import qualified D2C2
-- import qualified D3C1
-- import qualified D3C2
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
import System.Environment as Sys

solutions :: [[String -> String]]
solutions =
  [ [ D1C1.solve,
      D1C2.solve
    ],
    [ D2C1.solve,
      D2C2.solve
    ]
    -- ,[
    --  D2C1.solve
    --  ,D2C2.solve
    -- ]
    -- ,[
    --  D3C1.solve
    --  ,D3C2.solve
    -- ]
    -- ,[
    --  D4C1.solve
    --  ,D4C2.solve
    -- ]
    -- ,[
    --  D5C1.solve
    --  ,D5C2.solve
    -- ]
    -- ,[
    --  D6C1.solve
    --  ,D6C2.solve
    -- ]
    -- ,[
    --  D7C1.solve
    --  ,D7C2.solve
    -- ]
    -- ,[
    --  D8C1.solve
    --  ,D8C2.solve
    -- ]
    -- ,[
    --  D9C1.solve
    --  ,D9C2.solve
    -- ]
    -- ,[
    --  D10C1.solve
    --  ,D10C2.solve
    -- ]
    -- ,[
    --  D11C1.solve
    --  ,D11C2.solve
    -- ]
    -- ,[
    --  D12C1.solve
    --  ,D12C2.solve
    -- ]
  ]

files :: [String]
files =
  [ "inputs/1.in",
    "inputs/2.in"
  ]

main :: IO ()
main = do
  args <- Sys.getArgs
  if length args /= 2
    then print "usage: \"./app 4 2\" for day 4 question 2"
    else do
      let (d : c : _) = map (read :: String -> Int) args
      input <- readFile $ files !! (d - 1)
      print $ solutions !! (d - 1) !! (c - 1) $ input