module Encoder where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Bimap as B
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

emptyEnv :: Environment
emptyEnv = Env B.empty [] []

populate :: Problem -> Environment
populate (servers, vms) = Env bimap servers vms
  where
    bimap = B.fromList $ zip [1..] [(s,v) | s <- servers, v <- vms]

moreLits :: Int -> Maybe (Encoder (Int,Int))
moreLits x
  | x >= 0 = Just $ lift $ do
      n <- get
      put (n + x)
      return (n + 1, n + x)
  | otherwise = Nothing

newLit :: Encoder Int
newLit = lift $ do
  n <- get
  put (n+1)
  return (n+1)

encode :: Environment -> Encoder a -> a
encode env encoder = evalState (runReaderT encoder env) litCount
  where litCount = B.size (bimap env)
