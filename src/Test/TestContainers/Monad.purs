module Test.TestContainers.Monad
  ( WithContainer
  , configure
  , getContainer
  , setExposedPortsM
  , setPullPolicyM
  , setCommandM
  , setEntrypointM
  , setEnvironmentM
  , setLabelsM
  , setBindMountsM
  , setNameM
  , setCopyFilesToContainerM
  , setWorkingDirectoryM
  , setDefaultLogDriverM
  , setTmpFsM
  , setUserM
  , setPrivilegedModeM
  , setAddedCapabilitiesM
  , setDroppedCapabilitiesM
  , setIpcModeM
  , setResourcesQuotaM
  , setSharedMemorySizeM
  , setReuseM
  , setWaitStrategyM
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Test.TestContainers (setAddedCapabilities, setBindMounts, setCommand, setCopyFilesToContainer, setDefaultLogDriver, setDroppedCapabilities, setEntrypoint, setEnvironment, setExposedPorts, setIpcMode, setLabels, setName, setPrivilegedMode, setPullPolicy, setResourcesQuota, setReuse, setSharedMemorySize, setTmpFs, setUser, setWaitStrategy, setWorkingDirectory)
import Test.TestContainers.Types (BindMounts, Capability, CopyContentToContainer, FilePath, IPCMode, KV, MemorySize, PullPolicy, ResourcesQuota, TestContainer, TmpFS, User, WaitStrategy)

newtype WithContainer a = WithContainer { runConfiguration :: TestContainer -> Tuple a TestContainer }

configure :: ∀ a. TestContainer -> WithContainer a -> a
configure tc (WithContainer a) =
  let
    (Tuple res _) = a.runConfiguration tc
  in
    res

instance Functor WithContainer where
  map :: ∀ a b. (a -> b) -> WithContainer a -> WithContainer b
  map f (WithContainer f') = WithContainer $
    { runConfiguration: \tc ->
        let
          (Tuple res cnt) = f'.runConfiguration tc
        in
          Tuple (f res) cnt
    }

instance Apply WithContainer where
  apply :: ∀ a b. WithContainer (a -> b) -> WithContainer a -> WithContainer b
  apply (WithContainer f) (WithContainer val) = WithContainer $
    { runConfiguration: \tc ->
        let
          (Tuple ab cnt) = f.runConfiguration tc
          (Tuple res cnt') = val.runConfiguration cnt
        in
          Tuple (ab res) cnt'
    }

instance Applicative WithContainer where
  pure :: ∀ a. a -> WithContainer a
  pure x = WithContainer $ { runConfiguration: \tc -> Tuple x tc }

instance Bind WithContainer where
  bind :: ∀ a b. WithContainer a -> (a -> WithContainer b) -> WithContainer b
  bind (WithContainer a) func = WithContainer $
    { runConfiguration: \tc ->
        let
          (Tuple a' cnt) = a.runConfiguration tc
          (WithContainer b) = func a'
        in
          b.runConfiguration cnt
    }

instance Monad WithContainer

getContainer :: WithContainer TestContainer
getContainer = WithContainer $ { runConfiguration: \c -> Tuple c c }

setAndUnit :: (TestContainer -> TestContainer) -> WithContainer Unit
setAndUnit f = WithContainer $ { runConfiguration: \c -> Tuple unit (f c) }

setExposedPortsM :: Array Int -> WithContainer Unit
setExposedPortsM = setAndUnit <<< setExposedPorts

setPullPolicyM :: PullPolicy -> WithContainer Unit
setPullPolicyM = setAndUnit <<< setPullPolicy

setCommandM :: Array String -> WithContainer Unit
setCommandM = setAndUnit <<< setCommand

setEntrypointM :: Array String -> WithContainer Unit
setEntrypointM = setAndUnit <<< setEntrypoint

setEnvironmentM :: Array KV -> WithContainer Unit
setEnvironmentM = setAndUnit <<< setEnvironment

setLabelsM :: Array KV -> WithContainer Unit
setLabelsM = setAndUnit <<< setLabels

setBindMountsM :: Array BindMounts -> WithContainer Unit
setBindMountsM = setAndUnit <<< setBindMounts

setNameM :: String -> WithContainer Unit
setNameM = setAndUnit <<< setName

setCopyFilesToContainerM :: Array CopyContentToContainer -> WithContainer Unit
setCopyFilesToContainerM = setAndUnit <<< setCopyFilesToContainer

setWorkingDirectoryM :: FilePath -> WithContainer Unit
setWorkingDirectoryM = setAndUnit <<< setWorkingDirectory

setDefaultLogDriverM :: WithContainer Unit
setDefaultLogDriverM = setAndUnit $ setDefaultLogDriver

setTmpFsM :: TmpFS -> WithContainer Unit
setTmpFsM = setAndUnit <<< setTmpFs

setUserM :: User -> WithContainer Unit
setUserM = setAndUnit <<< setUser

setPrivilegedModeM :: WithContainer Unit
setPrivilegedModeM = setAndUnit $ setPrivilegedMode

setAddedCapabilitiesM :: Array Capability -> WithContainer Unit
setAddedCapabilitiesM = setAndUnit <<< setAddedCapabilities

setDroppedCapabilitiesM :: Array Capability -> WithContainer Unit
setDroppedCapabilitiesM = setAndUnit <<< setDroppedCapabilities

setIpcModeM :: IPCMode -> WithContainer Unit
setIpcModeM = setAndUnit <<< setIpcMode

setResourcesQuotaM :: ResourcesQuota -> WithContainer Unit
setResourcesQuotaM = setAndUnit <<< setResourcesQuota

setSharedMemorySizeM :: MemorySize -> WithContainer Unit
setSharedMemorySizeM = setAndUnit <<< setSharedMemorySize

setReuseM :: WithContainer Unit
setReuseM = setAndUnit $ setReuse

setWaitStrategyM :: Array WaitStrategy -> WithContainer Unit
setWaitStrategyM = setAndUnit <<< setWaitStrategy
