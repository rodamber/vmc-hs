module Problem
  ( module Server
  , module VM
  , Problem
  , view
  , servers
  , vms
  ) where

import Server
import VM

-- | Represents the Virtual Machine Consolidation problem. Has information about
-- the servers and the virtual machines.
type Problem = ([Server], [VM])

-- | Lists the servers and virtual machines of the problem.
view :: Problem -> ([Server], [VM])
view = id

-- | Lists just the servers.
-- prop> servers = fst . view
servers :: Problem -> [Server]
servers = fst

-- | Lists just the virtual machines.
-- prop> vms = snd . view
vms :: Problem -> [VM]
vms = snd
