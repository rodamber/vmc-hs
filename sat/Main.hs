module Main where

import Picosat

import Encoder
import Encodings

import Types

baseEncodings :: Encoder CNF
baseEncodings = concat <$> sequence
  [ atLeastOne
  , atMostOnePairwise
  , antiCollocation
  , capacityLimit -- here is the error
  , serverUpperLimit 2
  ]

main :: IO Solution
main  = solve $ fst $ encode (ss,vv) baseEncodings
