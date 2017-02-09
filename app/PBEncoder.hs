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

  let x = \i -> (map snd wxs) !! i
  let w = \i -> (map fst wxs) !! i
  let s = \i j -> [min..max] !! (i * k + j)

  return $
    -- 1. x(i) => s(i)(j) | forall i, j: 1 <= i <= n, 1 <= j <= w(i)
    [[-x(i), s(i)(j)] | i <- [1..n], j <- [1..w(i)]]

    -- 2. -s(1)(j) | forall j: w(1) < j <= k
    ++ [[s(1)(j)] | j <- [w(1)+1..k]]

    -- 3. s(i-1)(j) => s(i)(j) | forall i, j: 2 <= i <= n, 1 <= j <= k
    ++ [[-s(i-1)(j), s(i)(j)] | i <- [2..n], j <- [1..k]]

    -- 4. x(i) . s(i - 1)(j) => s(i)(j + w(i)) | forall i, j: 2 <= i <= n, 1 <= j <= k - w(i)
    ++ [[-x(i), -s(i-1)(j), s(i)(j+w(i))] | i <- [2..n], j <- [1..k-w(i)]]

    -- 5. x(i) => -s(i-1)(k+1-w(i)) | forall i: 2 <= i <= n
    ++ [[-x(i), -s(i-1)(k+1-w(i))] | i <- [2..n]]

