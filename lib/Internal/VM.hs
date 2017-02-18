module Internal.VM where

import Control.Monad.State
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.Monoid
import Data.Ord
import Data.Validation

-- | Represents a virtual machine (VM) in the domain.
data VM = VM
  { -- | Unique global ID.
    vmID_  :: Int
    -- | The job to which the VM belongs.
  , jobID_ :: Int
    -- | Unique ID inside among the VMs in the same job.
  , vmIndex_ :: Int
  -- | RAM requirement. Should be positive.
  , ramReq_ :: Int
  -- | CPU requirement. Should be positive.
  , cpuReq_ :: Int
  -- | A VM with an /anti-collocation/ constraint can't be placed in the same
  -- server as another VM belonging to the same job with an /anti-collocation/
  -- constraint.
  , antiCollocation_ :: Bool
  } deriving (Show)

-- | prop> v1 == v2 = vmID v1 == vmID v2
instance Eq VM where
  v1 == v2 = vmID v1 == vmID v2

-- | prop> compare = comparing vmID
instance Ord VM where
  compare = comparing vmID

-- | Unique global ID.
--
-- prop> vmID vm > 0
vmID :: VM -> Int
vmID = vmID_

-- | The job to which the VM belongs.
--
-- prop> jobID vm > 0
jobID :: VM -> Int
jobID = jobID_

-- | Index in the list of VMs of the job it is a member of.
--
-- prop> vmIndex vm > 0
vmIndex :: VM -> Int
vmIndex = vmIndex_

-- | CPU requirement of the VM.
--
-- prop> cpuReq vm > 0
cpuReq :: VM -> Int
cpuReq = cpuReq_

-- | RAM requirement. It is non-negative.
--
-- prop> ramReq vm > 0
ramReq :: VM -> Int
ramReq = ramReq_

-- | A VM with an /anti-collocation/ constraint can't be placed in the same
-- server as another VM belonging to the same job with an /anti-collocation/
-- constraint.
antiCollocation :: VM -> Bool
antiCollocation = antiCollocation_

-- | Simple type alias, nothing fancy.
type ID  = Int
-- | Simple type alias, nothing fancy.
type CPU = Int
-- | Simple type alias, nothing fancy.
type RAM = Int

-- | Unique VM ID store. Also mantains the mapping between the jobs and the
-- indices of the VMs already built. This state is used by VMBuilder to ensure
-- that the ID of each VM is unique and that the VMs have unique indices inside
-- each job.
data Env = Env
  { nextID      :: ID                  -- ^ Next available ID.
  , usedIndices :: (M.IntMap S.IntSet) -- ^ Mapping from jobs to VM indices
  }

-- | Initial VM building environment. IDs start at 0 and no VMs were built yet.
initialEnv :: Env
initialEnv = Env 0 M.empty

-- | Represents an error when building a VM.
data VMError =
    NegativeJobID ID    -- ^ Provided job id is negative
  | NegativeVMIndex ID  -- ^ Provided VM index is negative
  | DuplicateVMIndex ID -- ^ Provided VM index already exists
  | NegativeCPU CPU     -- ^ Provided CPU capacity is negative
  | NegativeRAM RAM     -- ^ Provided RAM capacity is negative
  deriving (Eq, Show)

-- | VM builder. Ensures the resulting VM is valid.
newtype VMBuilder = VMBuilder
  { runVMBuilder :: State Env (AccValidation [VMError] VM) }

-- | Ensures the real is non-negative.
nonNegative :: Real a => (a -> VMError) -> a -> AccValidation [VMError] a
nonNegative err x | x < 0 = AccFailure [err x] | otherwise = AccSuccess x

-- | Ensures the VM job id is non-negative.
nonNegativeJobID :: Int -> AccValidation [VMError] Int
nonNegativeJobID = nonNegative NegativeJobID

-- | Ensures the VM index in its job is non-negative.
nonNegativeVMIndex :: Int -> AccValidation [VMError] Int
nonNegativeVMIndex = nonNegative NegativeVMIndex

-- | Ensures the VM CPU requirement is non-negative.
nonNegativeCPUReq :: Int -> AccValidation [VMError] Int
nonNegativeCPUReq = nonNegative NegativeCPU

-- | Ensures the VM RAM requirement is non-negative.
nonNegativeRAMReq :: Int -> AccValidation [VMError] Int
nonNegativeRAMReq = nonNegative NegativeRAM

-- | Ensures that the index is unique __in the same job__.
uniqueVMIndex :: ID -- ^ Job id
              -> Int -- ^ VM index in that job
              -> State Env (AccValidation [VMError] Int)
uniqueVMIndex id' ix = do
  indices <- gets usedIndices
  case M.lookup id' indices of
    -- This job doesn't have any VMs yet, so we create an entry for it.
    Nothing -> do
      modify $ \s -> s { usedIndices = M.insert id' (S.singleton ix) indices}
      return $ AccSuccess ix
    -- The job already has VMs, so let's check if the index already exists.
    Just jobIndices -> return $
      if S.member ix jobIndices
      then AccFailure [DuplicateVMIndex ix]
      else AccSuccess ix

-- | Adds a new VM index to the used indices of that job. __Does not check__ if
-- it is unique.
addVMIndex :: ID -- ^ ID of the job in question
           -> Int -- ^ VM index in that job
           -> State Env ()
addVMIndex id' ix = modify $ \s ->
  s { usedIndices = M.insertWith (S.union) id' (S.singleton ix) (usedIndices s) }

-- | Generates /unique/ VM IDs.
genID :: State Env Int
genID = do
  id' <- gets nextID
  modify $ \s -> s { nextID = id' + 1}
  return id'

-- | Builds a new /valid/ VM, i.e.:
--
-- * VM ID, job ID, VM index and the requirements are non-negative;
-- * VM ID is unique;
-- * VM index is unique among the VMs of its job.
buildVM :: ID -> Int -> RAM -> CPU -> Bool -> VMBuilder
buildVM jobID' vmIndex' ramReq' cpuReq' antiCol' = VMBuilder $ do
  vmID' <- genID
  uniqueVMIndex' <- uniqueVMIndex jobID' vmIndex'

  let vmValidation = VM vmID'
        <$> (nonNegativeJobID jobID')
        <*> (uniqueVMIndex' <> nonNegativeVMIndex vmIndex')
        <*> (nonNegativeCPUReq cpuReq')
        <*> (nonNegativeRAMReq ramReq')
        <*> (AccSuccess antiCol')

  when (isSuccess vmValidation) (addVMIndex jobID' vmIndex')
  return vmValidation

  where
    isSuccess (AccFailure _) = False
    isSuccess (AccSuccess _) = True

-- mkVM :: VMBuilder -> AccValidation [Env] VM
-- mkVM = runVMBuilder . flip evalState
