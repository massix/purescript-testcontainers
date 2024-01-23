module Test.Testcontainers
  ( exec
  , getFirstMappedPort
  , getHost
  , getId
  , getMappedPort
  , getName
  , mkContainer
  , mkContainerFromDockerfile
  , mkContainerFromDockerfile'
  , mkContainerFromDockerfileOpts
  , restartContainer
  , setAddedCapabilities
  , setBindMounts
  , setCommand
  , setCopyFilesToContainer
  , setDefaultLogDriver
  , setDroppedCapabilities
  , setEntrypoint
  , setEnvironment
  , setExposedPorts
  , setExtraHosts
  , setIpcMode
  , setLabels
  , setName
  , setNetwork
  , setNetworkAliases
  , setNetworkMode
  , setPrivilegedMode
  , setPullPolicy
  , setResourcesQuota
  , setReuse
  , setSharedMemorySize
  , setTmpFs
  , setUser
  , setStartupTimeout
  , setWaitStrategy
  , setWorkingDirectory
  , startContainer
  , stopContainer
  , withContainer
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Test.Testcontainers.Types (class IsImage, BindMounts, Capability, CopyContentToContainer, ExecResult, ExtraHost, FilePath, GenericContainer, IPCMode, Image(..), KV, MemorySize, Network(..), NetworkMode(..), PullPolicy, ResourcesQuota, StartedTestContainer, StartupTimeout, StoppedTestContainer, TestContainer(..), TmpFS, User, WaitStrategy, capToString, toImage)

foreign import mkContainerImpl :: (GenericContainer -> TestContainer) -> String -> TestContainer
foreign import mkContainerFromDockerfileImpl :: (GenericContainer -> TestContainer) -> String -> String ->  Effect (Promise TestContainer)
foreign import mkContainerFromDockerfileCustomDockerfileImpl :: (GenericContainer -> TestContainer) -> String -> String -> String -> Effect (Promise TestContainer)
foreign import mkContainerFromDockerfileOptsImpl
  :: (GenericContainer -> TestContainer) --^ constructor
  -> String --^ ContextPath
  -> Maybe String --^ Dockerfile
  -> Maybe PullPolicy --^ Pull Policy
  -> String --^ Image
  -> Maybe (Array KV) --^ Build Args
  -> Maybe Boolean --^ Cache
  -> (String -> Either String TestContainer) --^ Left
  -> (TestContainer -> Either String TestContainer) --^ Right
  -> Effect (Promise (Either String TestContainer))

foreign import setExposedPortsImpl :: TestContainer -> (GenericContainer -> TestContainer) -> Array Int -> TestContainer
foreign import setPullPolicyImpl :: TestContainer -> (GenericContainer -> TestContainer) -> PullPolicy -> TestContainer
foreign import setCommandImpl :: TestContainer -> (GenericContainer -> TestContainer) -> Array String -> TestContainer
foreign import setEntrypointImpl :: TestContainer -> (GenericContainer -> TestContainer) -> Array String -> TestContainer
foreign import setEnvironmentImpl :: TestContainer -> (GenericContainer -> TestContainer) -> Array KV -> TestContainer
foreign import setLabelsImpl :: TestContainer -> (GenericContainer -> TestContainer) -> Array KV -> TestContainer
foreign import setBindMountsImpl :: TestContainer -> (GenericContainer -> TestContainer) -> Array BindMounts -> TestContainer
foreign import setNameImpl :: TestContainer -> (GenericContainer -> TestContainer) -> String -> TestContainer
foreign import setCopyFilesToContainerImpl :: TestContainer -> (GenericContainer -> TestContainer) -> Array CopyContentToContainer -> TestContainer
foreign import setWorkingDirectoryImpl :: TestContainer -> (GenericContainer -> TestContainer) -> FilePath -> TestContainer
foreign import setDefaultLogDriverImpl :: TestContainer -> (GenericContainer -> TestContainer) -> TestContainer
foreign import setTmpFsImpl :: TestContainer -> (GenericContainer -> TestContainer) -> TmpFS -> TestContainer
foreign import setUserImpl :: TestContainer -> (GenericContainer -> TestContainer) -> User -> TestContainer
foreign import setPrivilegedModeImpl :: TestContainer -> (GenericContainer -> TestContainer) -> TestContainer
foreign import setAddedCapabilitiesImpl :: TestContainer -> (GenericContainer -> TestContainer) -> Array String -> TestContainer
foreign import setDroppedCapabilitiesImpl :: TestContainer -> (GenericContainer -> TestContainer) -> Array String -> TestContainer
foreign import setIpcModeImpl :: TestContainer -> (GenericContainer -> TestContainer) -> String -> TestContainer
foreign import setResourcesQuotaImpl :: TestContainer -> (GenericContainer -> TestContainer) -> ResourcesQuota -> TestContainer
foreign import setSharedMemorySizeImpl :: TestContainer -> (GenericContainer -> TestContainer) -> MemorySize -> TestContainer
foreign import setReuseImpl :: TestContainer -> (GenericContainer -> TestContainer) -> TestContainer
foreign import setStartupTimeoutImpl :: TestContainer -> (GenericContainer -> TestContainer) -> StartupTimeout -> TestContainer
foreign import setWaitStrategyImpl :: TestContainer -> (GenericContainer -> TestContainer) -> Array WaitStrategy -> TestContainer
foreign import setNetworkModeImpl :: TestContainer -> (GenericContainer -> TestContainer) -> String -> TestContainer
foreign import setExtraHostsImpl :: TestContainer -> (GenericContainer -> TestContainer) -> Array ExtraHost -> TestContainer
foreign import setNetworkAliasesImpl :: TestContainer -> (GenericContainer -> TestContainer) -> Array String -> TestContainer
foreign import setNetworkImpl :: TestContainer -> (GenericContainer -> TestContainer) -> Network -> TestContainer
foreign import getIdImpl :: TestContainer -> (String -> Either String String) -> (String -> Either String String) -> Effect (Either String String)
foreign import getNameImpl
  :: TestContainer
  -> (String -> Either String String)
  -> (String -> Either String String)
  -> Effect (Either String String)

foreign import getHostImpl
  :: TestContainer
  -> (String -> Either String String)
  -> (String -> Either String String)
  -> Effect (Either String String)

foreign import execImpl
  :: TestContainer
  -> Array String
  -> (String -> Either String ExecResult)
  -> (ExecResult -> Either String ExecResult)
  -> Effect (Promise (Either String ExecResult))

foreign import getFirstMappedPortImpl
  :: TestContainer
  -> (String -> Either String Int)
  -> (Int -> Either String Int)
  -> Effect (Either String Int)

foreign import getMappedPortImpl
  :: TestContainer
  -> Int
  -> (String -> Either String Int)
  -> (Int -> Either String Int)
  -> Effect (Either String Int)

foreign import startContainerImpl
  :: TestContainer
  -> (StartedTestContainer -> TestContainer)
  -> (TestContainer -> Either String TestContainer)
  -> (String -> Either String TestContainer)
  -> Effect (Promise (Either String TestContainer))

foreign import stopContainerImpl
  :: TestContainer
  -> (StoppedTestContainer -> TestContainer)
  -> (TestContainer -> Either String TestContainer)
  -> (String -> Either String TestContainer)
  -> Effect (Promise (Either String TestContainer))

foreign import restartContainerImpl
  :: TestContainer
  -> (StartedTestContainer -> TestContainer)
  -> (TestContainer -> Either String TestContainer)
  -> (String -> Either String TestContainer)
  -> Effect (Promise (Either String TestContainer))

-- | Smart constructor to build a new StoppedTestContainer
mkContainer :: ∀ a. IsImage a => a -> TestContainer
mkContainer i =
  let
    s@(Image orig) = toImage i
  in
    mkContainerImpl (GenericContainer s) orig

mkContainerFromDockerfileOpts
  :: ∀ a. IsImage a
   => FilePath
   -> Maybe String
   -> Maybe PullPolicy
   -> a
   -> Maybe (Array KV)
   -> Maybe Boolean
   -> Aff (Either String TestContainer)
mkContainerFromDockerfileOpts context fp pp img buildArgs cache =
  let
    s@(Image orig) = toImage img
  in
    toAffE $ mkContainerFromDockerfileOptsImpl (GenericContainer s) context fp pp orig buildArgs cache Left Right

mkContainerFromDockerfile :: ∀ a. IsImage a => FilePath -> a -> Aff TestContainer
mkContainerFromDockerfile fp img =
  let
    s@(Image orig) = toImage img
  in
    toAffE $ mkContainerFromDockerfileImpl (GenericContainer s) fp orig

mkContainerFromDockerfile' :: ∀ a. IsImage a => FilePath -> FilePath -> a -> Aff TestContainer
mkContainerFromDockerfile' context dockerFile img =
  let
    s@(Image orig) = toImage img
  in
    toAffE $ mkContainerFromDockerfileCustomDockerfileImpl (GenericContainer s) context dockerFile orig

-- | Add an exposed port to the container
setExposedPorts :: Array Int -> TestContainer -> TestContainer
setExposedPorts ports c@(GenericContainer i _) = setExposedPortsImpl c (GenericContainer i) ports
setExposedPorts _ c = c

-- | Whether to always Pull the image or use the Default strategy
setPullPolicy :: PullPolicy -> TestContainer -> TestContainer
setPullPolicy p c@(GenericContainer i _) = setPullPolicyImpl c (GenericContainer i) p
setPullPolicy _ c = c

-- | Arguments to pass to the entrypoint (if any)
setCommand :: Array String -> TestContainer -> TestContainer
setCommand cmd c@(GenericContainer i _) = setCommandImpl c (GenericContainer i) cmd
setCommand _ c = c

-- | Configure the entry point for the container
setEntrypoint :: Array String -> TestContainer -> TestContainer
setEntrypoint ep c@(GenericContainer i _) = setEntrypointImpl c (GenericContainer i) ep
setEntrypoint _ c = c

-- | Environment variables to set
setEnvironment :: Array KV -> TestContainer -> TestContainer
setEnvironment env c@(GenericContainer i _) = setEnvironmentImpl c (GenericContainer i) env
setEnvironment _ c = c

-- | Labels to set
setLabels :: Array KV -> TestContainer -> TestContainer
setLabels labels c@(GenericContainer i _) = setLabelsImpl c (GenericContainer i) labels
setLabels _ c = c

-- | Bind mount volumes
setBindMounts :: Array BindMounts -> TestContainer -> TestContainer
setBindMounts binds c@(GenericContainer i _) = setBindMountsImpl c (GenericContainer i) binds
setBindMounts _ c = c

-- | Sets the name of the container (Warning: using this is dangerous!)
setName :: String -> TestContainer -> TestContainer
setName name c@(GenericContainer i _) = setNameImpl c (GenericContainer i) name
setName _ c = c

-- | Copy files or directories to the container
setCopyFilesToContainer :: Array CopyContentToContainer -> TestContainer -> TestContainer
setCopyFilesToContainer files c@(GenericContainer i _) = setCopyFilesToContainerImpl c (GenericContainer i) files
setCopyFilesToContainer _ c = c

-- | Sets the working directory
setWorkingDirectory :: FilePath -> TestContainer -> TestContainer
setWorkingDirectory wd c@(GenericContainer i _) = setWorkingDirectoryImpl c (GenericContainer i) wd
setWorkingDirectory _ c = c

-- | Use the default log driver instead of the Ryuk's based one
setDefaultLogDriver :: TestContainer -> TestContainer
setDefaultLogDriver c@(GenericContainer i _) = setDefaultLogDriverImpl c (GenericContainer i)
setDefaultLogDriver c = c

-- | Create new tmpfs mounts inside the container
setTmpFs :: TmpFS -> TestContainer -> TestContainer
setTmpFs tmpFs c@(GenericContainer i _) = setTmpFsImpl c (GenericContainer i) tmpFs
setTmpFs _ c = c

-- | Sets the user to run as
setUser :: User -> TestContainer -> TestContainer
setUser u c@(GenericContainer i _) = setUserImpl c (GenericContainer i) u
setUser _ c = c

-- | Whether to run in privileged mode
setPrivilegedMode :: TestContainer -> TestContainer
setPrivilegedMode c@(GenericContainer i _) = setPrivilegedModeImpl c (GenericContainer i)
setPrivilegedMode c = c

-- | Add capabilities
setAddedCapabilities :: Array Capability -> TestContainer -> TestContainer
setAddedCapabilities cap c@(GenericContainer i _) = setAddedCapabilitiesImpl c (GenericContainer i) $ capToString <$> cap
setAddedCapabilities _ c = c

-- | Drop capabilities
setDroppedCapabilities :: Array Capability -> TestContainer -> TestContainer
setDroppedCapabilities cap c@(GenericContainer i _) = setDroppedCapabilitiesImpl c (GenericContainer i) $ capToString <$> cap
setDroppedCapabilities _ c = c

-- | Configure the IPC mode
setIpcMode :: IPCMode -> TestContainer -> TestContainer
setIpcMode mode c@(GenericContainer i _) = setIpcModeImpl c (GenericContainer i) $ show mode
setIpcMode _ c = c

-- | Set the quota for the resources
setResourcesQuota :: ResourcesQuota -> TestContainer -> TestContainer
setResourcesQuota r c@(GenericContainer i _) = setResourcesQuotaImpl c (GenericContainer i) r
setResourcesQuota _ c = c

-- | Set the shared memory size
setSharedMemorySize :: MemorySize -> TestContainer -> TestContainer
setSharedMemorySize r c@(GenericContainer i _) = setSharedMemorySizeImpl c (GenericContainer i) r
setSharedMemorySize _ c = c

-- | Whether to re-use the container
setReuse :: TestContainer -> TestContainer
setReuse c@(GenericContainer i _) = setReuseImpl c (GenericContainer i)
setReuse c = c

-- | Configure the startup timeout
setStartupTimeout :: StartupTimeout -> TestContainer -> TestContainer
setStartupTimeout t c@(GenericContainer i _) = setStartupTimeoutImpl c (GenericContainer i) t
setStartupTimeout _ c = c

-- | Configure the wait strategy
setWaitStrategy :: Array WaitStrategy -> TestContainer -> TestContainer
setWaitStrategy w c@(GenericContainer i _) = setWaitStrategyImpl c (GenericContainer i) w
setWaitStrategy _ c = c

-- | Sets the network mode
setNetworkMode :: NetworkMode -> TestContainer -> TestContainer
setNetworkMode (NetworkMode n) c@(GenericContainer i _) = setNetworkModeImpl c (GenericContainer i) n
setNetworkMode _ c = c

setExtraHosts :: Array ExtraHost -> TestContainer -> TestContainer
setExtraHosts hosts c@(GenericContainer i _) = setExtraHostsImpl c (GenericContainer i) hosts
setExtraHosts _ c = c

setNetworkAliases :: Array String -> TestContainer -> TestContainer
setNetworkAliases aliases c@(GenericContainer i _) = setNetworkAliasesImpl c (GenericContainer i) aliases
setNetworkAliases _ c = c

setNetwork :: Network -> TestContainer -> TestContainer
setNetwork n@(StartedNetwork _) c@(GenericContainer i _) = setNetworkImpl c (GenericContainer i) n
setNetwork _ c = c

-- | Get the mapped port for the running container
getMappedPort :: ∀ m. MonadEffect m => Int -> TestContainer -> m (Either String Int)
getMappedPort p c@(StartedTestContainer _ _) = liftEffect $ getMappedPortImpl c p Left Right
getMappedPort _ _ = pure $ Left "Could not get mapped port for this container"

-- | If the container only exposes a single port, this will return that
getFirstMappedPort :: ∀ m. MonadEffect m => TestContainer -> m (Either String Int)
getFirstMappedPort c@(StartedTestContainer _ _) = liftEffect $ getFirstMappedPortImpl c Left Right
getFirstMappedPort _ = pure $ Left "Could not get mapped port for this container"

-- | Launch an arbitrary command inside the container
exec :: ∀ m. MonadAff m => Array String -> TestContainer -> m (Either String ExecResult)
exec cmds c@(StartedTestContainer _ _) = liftAff $ toAffE $ execImpl c cmds Left Right
exec _ _ = pure $ Left "Cannot exec on a non-started container"

-- | Return the name of the container
getName :: ∀ m. MonadEffect m => TestContainer -> m (Either String String)
getName c@(StartedTestContainer _ _) = liftEffect $ getNameImpl c Left Right
getName _ = pure $ Left "Can only get the name of a started container"

-- | Retrieve the configured host of the container
getHost :: ∀ m. MonadEffect m => TestContainer -> m (Either String String)
getHost c@(StartedTestContainer _ _) = liftEffect $ getHostImpl c Left Right
getHost _ = pure $ Left "Can only get the host of a started container"

-- | Get the ID of a running container
getId :: ∀ m. MonadEffect m => TestContainer -> m (Either String String)
getId c@(StartedTestContainer _ _) = liftEffect $ getIdImpl c Left Right
getId c@(StoppedTestContainer _ _) = liftEffect $ getIdImpl c Left Right
getId _ = pure $ Left "Can only get the ID of a started or stopped container"

-- | Starts a stopped container
startContainer :: ∀ m. MonadAff m => TestContainer -> m (Either String TestContainer)
startContainer (StartedTestContainer _ _) = pure $ Left "Already started"
startContainer (StoppedTestContainer _ _) = pure $ Left "Cannot start a stopped container, create a new one!"
startContainer c = liftAff $ toAffE $ startContainerImpl c (StartedTestContainer $ getImage c) Right Left
  where
  getImage :: TestContainer -> Image
  getImage (StartedTestContainer i _) = i
  getImage (StoppedTestContainer i _) = i
  getImage (GenericContainer i _) = i

-- | Restarts a started container
restartContainer :: ∀ m. MonadAff m => TestContainer -> m (Either String TestContainer)
restartContainer c@(StartedTestContainer i _) = liftAff $ toAffE $ restartContainerImpl c (StartedTestContainer i) Right Left
restartContainer _ = pure $ Left "Already started"

-- | Stops a started container
stopContainer :: ∀ m. MonadAff m => TestContainer -> m (Either String TestContainer)
stopContainer c@(StartedTestContainer i _) = liftAff $ toAffE $ stopContainerImpl c (StoppedTestContainer i) Right Left
stopContainer _ = pure $ Left "Already stopped"

-- | Bracket
withContainer :: ∀ m a. (MonadAff m) => TestContainer -> (TestContainer -> m a) -> m (Either String a)
withContainer c@(GenericContainer _ _) act = do
  startRes <- startContainer c
  case startRes of
    Left e -> pure $ Left e
    Right x -> do
      res <- act x
      void $ stopContainer x
      pure $ Right res
withContainer _ _ = pure $ Left "This only works with GenericContainer"
