module Encoder where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Bimap as B
import Data.Function (on)
import Data.List (groupBy, tails)
import Data.Maybe (fromJust)

import Types

type Lit = Int
type LitCount = Int

type CNF = [Clause]
type Clause = [Lit]

data Environment = Env
  { bimap :: B.Bimap Lit (Server, VM)
  , servers :: [Server]
  , vms :: [VM]
  } deriving Show

type Encoder = ReaderT Environment (State LitCount)

lookupLit :: B.Bimap Lit (Server, VM) -> Server -> VM -> Lit
lookupLit bm s v = fromJust (B.lookupR (s,v) bm)

populate :: Problem -> Environment
populate (servers, vms) = Env bimap servers vms
  where
    bimap = B.fromList $ zip [1..] [(s,v) | s <- servers, v <- vms]

atLeastOne :: Encoder CNF
atLeastOne = do
  (Env bimap servers vms) <- ask
  lift $ return [[lookupLit bimap s v | s <- servers] | v <- vms]

atMostOnePairwise :: Encoder CNF
atMostOnePairwise = do
  (Env bimap servers vms) <- ask
  lift $ return $ do
    (s1,s2) <- pairs servers
    v       <- vms

    let lit1 = lookupLit bimap s1 v
    let lit2 = lookupLit bimap s2 v

    return [-lit1, -lit2]

pairs :: [a] -> [(a,a)]
pairs xs = concat $ zipWith zip (map repeat xs) (tails (tail xs))

antiCollocation :: Encoder CNF
antiCollocation = do
  (Env bimap servers vms) <- ask
  (min,max) <- fromJust $ moreLits (length vms)

  let acLit = \vm -> [min..max] !! (vmID vm)

  let acCNF = do
        v <- vms
        let sig = if hasAntiCollocation v then 1 else -1
        return (sig * acLit v)

  lift $ return $ do
    s <- servers
    j <- jobs vms
    (v1,v2) <- pairs j

    let lit1 = lookupLit bimap s v1
    let lit2 = lookupLit bimap s v2

    return (acCNF ++ [-(acLit v1), -(acLit v2), -lit1, -lit2])

moreLits :: Int -> Maybe (Encoder (Int,Int))
moreLits x
  | x > 0 = Just $ lift $ do
      n <- get
      put (n + x)
      return (n + 1, n + x)
  | otherwise = Nothing
               
jobs :: [VM] -> [[VM]]
jobs = groupBy ((==) `on` jobID)

encode :: Problem -> (CNF, LitCount)
encode p =  runState (runReaderT encoding env) litCount
  where
    env = populate p
    encoding = concat <$> sequence [ atLeastOne
                                   , atMostOnePairwise
                                   , antiCollocation
                                   ]
    litCount = B.size (bimap env)

