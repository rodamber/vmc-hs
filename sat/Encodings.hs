module Encodings where

import Control.Monad.Trans.Reader (ask)
import Data.Function (on)
import Data.List (groupBy, sortBy, tails)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import Encoder
import PBEncoder
import Types

--------------------------------------------------------------------------------

atLeastOne' :: [Lit] -> CNF
atLeastOne' = return

atLeastOne :: Encoder CNF
atLeastOne = do
  Env bimap servers vms <- ask
  return $ do
    v <- vms
    atLeastOne' $ map (flip (lookupLit bimap) v) servers

--------------------------------------------------------------------------------

atMostOnePairwise' :: [Lit] -> CNF
atMostOnePairwise' lits = [[-l1, -l2] | (l1,l2) <- pairs lits]

pairs :: [a] -> [(a,a)]
pairs xs = concat $ zipWith zip (map repeat xs) (tails (tail xs))

atMostOnePairwise :: Encoder CNF
atMostOnePairwise = do
  Env bimap servers vms <- ask
  return $ do
    v <- vms
    let lits = map (flip (lookupLit bimap) v) servers
    atMostOnePairwise' lits

--------------------------------------------------------------------------------

-- At most one **pairwise** encoding of anti-collocation vms per server.
antiCollocationPairwise :: Encoder CNF
antiCollocationPairwise = do
  Env bimap servers vms <- ask

  return $ do
    s <- servers
    j <- jobs vms

    let acLits = map (lookupLit bimap s) (filter hasAntiCollocation j)
    atMostOnePairwise' acLits

jobs :: [VM] -> [[VM]]
jobs = groupBy ((==) `on` jobID) . sortBy (comparing vmID)

--------------------------------------------------------------------------------

capacityLimit :: Encoder CNF
capacityLimit = do
  Env bimap servers vms <- ask

  let encode :: Hardware -> [Encoder CNF]
      encode hw = do
        s <- servers

        let wxs = do
              v <- vms
              return (req hw v, lookupLit bimap s v)

        return $ swc $ PB wxs (cap hw s)

  concat <$> sequence (encode CPU ++ encode RAM)

data Hardware = CPU | RAM

cap :: Hardware -> Server -> Int
cap CPU = cpuCap
cap RAM = ramCap

req :: Hardware -> VM -> Int
req CPU = cpuReq
req RAM = ramReq

--------------------------------------------------------------------------------

serverUpperLimit :: Int -> Encoder CNF
serverUpperLimit n = do
  Env bimap servers vms <- ask
  (min,max) <- fromJust $ moreLits (length servers)

  let on = \s -> [min..max] !! (serverID s)

  let turnedOn :: CNF
      turnedOn = do
        s <- servers
        v <- vms

        let lit = lookupLit bimap s v

        return [-lit, on s]

  let onLimit :: Encoder CNF
      onLimit = swc (PB [(1,on(s)) | s <- servers] n)

  (++ turnedOn) <$> onLimit
