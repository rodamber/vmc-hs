module Sat.Encodings where

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord

import Sat.Encoder
import Sat.PBEncoder
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

type Encoding = [Lit] -> Encoder CNF

pairwise :: Encoding
pairwise lits = return [[-l1, -l2] | (l1,l2) <- pairs lits]

pairs :: [a] -> [(a,a)]
pairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys]

bitwise :: Encoding
bitwise [] = return []
bitwise lits = do
  let newLitCount = ceiling (logBase 2 (genericLength lits))
  (min,max) <- fromJust $ moreLits newLitCount

  let vss = [[-x,x] | x <- [min..max]]
  let combs = sequence vss

  return $ concat $ zipWith f lits combs

  where
    f :: Lit -> [Lit] -> CNF
    f x (y:ys) = [-x,y] : f x ys
    f _ _ = []

pairs' :: (Eq a, Num a) => [a] -> [(a,a)]
pairs' = filter (\(x,y) -> x /= -y) . pairs

atMostOne :: Encoding -> Encoder CNF
atMostOne encoding = do
  Env bimap servers vms <- ask
  fmap concat . sequence $ do
    v <- vms
    let lits = map (flip (lookupLit bimap) v) servers
    return (encoding lits :: Encoder CNF)

--------------------------------------------------------------------------------

antiCollocation :: Encoding -> Encoder CNF
antiCollocation encoding = do
  Env bimap servers vms <- ask
  fmap concat . sequence $ do
    s <- servers
    j <- jobs vms
    let lits = map (lookupLit bimap s) (filter hasAntiCollocation j)
    return (encoding lits :: Encoder CNF)


jobs :: [VM] -> [[VM]]
jobs = groupBy ((==) `on` jobID) . sortBy (comparing vmID)

--------------------------------------------------------------------------------

capacityLimit :: PBEncoding -> Encoder CNF
capacityLimit encoding = do
  Env bimap servers vms <- ask

  let encode :: Hardware -> [Encoder CNF]
      encode hw = do
        s <- servers

        let wxs = do
              v <- vms
              return (req hw v, lookupLit bimap s v)

        return $ encoding $ PB wxs (cap hw s)

  concat <$> sequence (encode CPU ++ encode RAM)

data Hardware = CPU | RAM

cap :: Hardware -> Server -> Int
cap CPU = cpuCap
cap RAM = ramCap

req :: Hardware -> VM -> Int
req CPU = cpuReq
req RAM = ramReq

--------------------------------------------------------------------------------

serverUpperLimit :: Int -> PBEncoding -> Encoder CNF
serverUpperLimit n encoding = do
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
      onLimit = encoding (PB [(1,on(s)) | s <- servers] n)

  (++ turnedOn) <$> onLimit
