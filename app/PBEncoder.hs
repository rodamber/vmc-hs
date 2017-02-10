module PBEncoder where

import Control.Monad.Trans.Reader (ask)
import Data.Maybe (fromJust)

import Encoder
import Types

-- Pseudo-Boolean Expression
data PBExpr = PB [(Int, Lit)] Int

-- Sequential Weighted Counter
swc :: PBExpr -> Encoder CNF
swc (PB wxs k) = do
  let n = length wxs
  (min,max) <- fromJust $ moreLits (k * n)

  let x i = (map snd wxs) !! i
  let w i = (map fst wxs) !! i
  let s i j = [min..max] !! (i * k + j)

  return $
    [[- x i, s i j] | i <- [1..n], j <- [1..w i]]
    ++ [[s 1 j] | j <- [w 1 + 1..k]]
    ++ [[-s (i-1) j , s i j] | i <- [2..n], j <- [1..k]]
    ++ [[- x i, -s (i-1) j, s i (j + w i)] | i <- [2..n], j <- [1..k - w i]]
    ++ [[- x i, -s (i-1) (k + 1 - w i)] | i <- [2..n]]

