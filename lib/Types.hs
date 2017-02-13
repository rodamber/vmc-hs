module Types where

import Data.Ord (comparing)

type Problem = ([Server], [VM])

data Server = Server
  { serverID :: Integer
  , ramCap   :: Integer -- RAM capacity
  , cpuCap   :: Integer -- CPU capacity
  } deriving (Eq)

instance Show Server where
  show server = "S" ++ show (serverID server)

instance Ord Server where
  compare = comparing serverID

data VM = VM
  { vmID               :: Integer
  , jobID              :: Integer -- The job to which this VM belongs
  , vmIndex            :: Integer -- The index of this VM in its job
  , ramReq             :: Integer -- RAM requirement
  , cpuReq             :: Integer -- CPU requirement
  , hasAntiCollocation :: Bool
  } deriving (Eq)

instance Show VM where
  show vm = "VM" ++ show (vmID vm)

instance Ord VM where
  compare = comparing vmID

-- Example 01.in

s00 = Server 0 5 2
s01 = Server 1 4 1
s02 = Server 2 7 3
s03 = Server 3 8 5

ss = [s00, s01, s02, s03]

vm00 = VM 0 0 0 1 1 True
vm01 = VM 1 0 1 1 1 True
vm02 = VM 2 1 0 1 1 False
vm03 = VM 3 2 0 1 1 True
vm04 = VM 4 2 1 1 1 True
vm05 = VM 5 2 2 1 1 False
vm06 = VM 6 3 0 1 1 True
vm07 = VM 7 3 1 1 1 True

vv = [vm00, vm01, vm02, vm03, vm04, vm05, vm06, vm07]
