module Server where

import Data.Ord

-- | Represents a server in the domain.
data Server = Server
  { serverID :: Int -- ^ Unique global ID.
  , ramCap   :: Int -- ^ RAM capacity. Should be positive.
  , cpuCap   :: Int -- ^ CPU capacity. Should be positive.
  } deriving (Eq, Show)

-- instance Show Server where
--   show server = "S" ++ show (serverID server)

instance Ord Server where
  compare = comparing serverID

