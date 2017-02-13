module Types where

import Data.Ord (comparing)

type Problem = ([Server], [VM])

data Server = Server
  { serverID :: Int
  , ramCap   :: Int -- RAM capacity
  , cpuCap   :: Int -- CPU capacity
  } deriving (Eq)

instance Show Server where
  show server = "S" ++ show (serverID server)

instance Ord Server where
  compare = comparing serverID

data VM = VM
  { vmID               :: Int
  , jobID              :: Int -- The job to which this VM belongs
  , vmIndex            :: Int -- The index of this VM in its job
  , ramReq             :: Int -- RAM requirement
  , cpuReq             :: Int -- CPU requirement
  , hasAntiCollocation :: Bool
  } deriving (Eq)

instance Show VM where
  show vm = "VM" ++ show (vmID vm)

instance Ord VM where
  compare = comparing vmID
