module Encodings
  ( atLeastOne
  , atMostOnePairwise
  , antiCollocation
  , capacityLimit
  , serverUpperLimit
  ) where

import Control.Monad.Trans.Reader (ask)
import Data.Function (on)
import Data.List (groupBy, tails)
import Data.Maybe (fromJust)

import Encoder
import PBEncoder
import Types

atLeastOne :: Encoder CNF
atLeastOne = do
  Env bimap servers vms <- ask
  return [[lookupLit bimap s v | s <- servers] | v <- vms]

atMostOnePairwise :: Encoder CNF
atMostOnePairwise = do
  Env bimap servers vms <- ask
  return $ do
    (s1,s2) <- pairs servers
    v       <- vms

    let lit1 = lookupLit bimap s1 v
    let lit2 = lookupLit bimap s2 v

    return [-lit1, -lit2]

pairs :: [a] -> [(a,a)]
pairs xs = concat $ zipWith zip (map repeat xs) (tails (tail xs))

antiCollocation :: Encoder CNF
antiCollocation = do
  Env bimap servers vms <- ask
  (min,max) <- fromJust $ moreLits (length vms)

  let acLit vm = [min..max] !! (vmID vm)

  let acCNF :: Clause
      acCNF = do
        v <- vms
        let sig = if hasAntiCollocation v then 1 else -1
        return (sig * acLit v)

  let acClauses :: [Clause]
      acClauses = do
        s <- servers
        j <- jobs vms
        (v1,v2) <- pairs j

        let lit1 = lookupLit bimap s v1
        let lit2 = lookupLit bimap s v2

        return [-(acLit v1), -(acLit v2), -lit1, -lit2]

  return $ acCNF : acClauses


jobs :: [VM] -> [[VM]]
jobs = groupBy ((==) `on` jobID)

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
