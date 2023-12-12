module Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, joinFiber, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Test.TestContainers (exec, getMappedPort, mkContainer, restartContainer, setAddedCapabilities, setBindMounts, setCopyFilesToContainer, setDefaultLogDriver, setDroppedCapabilities, setEnvironment, setExposedPorts, setIpcMode, setLabels, setName, setPrivilegedMode, setPullPolicy, setResourcesQuota, setReuse, setSharedMemorySize, setTmpFs, setUser, setWaitStrategy, setWorkingDirectory, startContainer, stopContainer, withContainer)
import Test.TestContainers.Monad (configure, getContainer, setAddedCapabilitiesM, setCommandM, setEntrypointM, setEnvironmentM, setNameM, setPrivilegedModeM, setPullPolicyM, setResourcesQuotaM, setReuseM, setUserM, setWaitStrategyM, setWorkingDirectoryM)
import Test.TestContainers.Types (Capability(..), CopyContentToContainer(..), IPCMode(..), PullPolicy(..), TestContainer, WaitStrategy(..))

monadicMain :: Aff Unit
monadicMain = do
  let
    cnt = configure (mkContainer "alpine:latest") $ do
      setEntrypointM [ "" ]
      setCommandM [ "sleep", "360" ]
      setNameM "sleeper"
      setUserM "root"
      setResourcesQuotaM { cpu: 0.1, memory: 0.5 }
      setPullPolicyM AlwaysPull
      setEnvironmentM [ { key: "ECHO_RESULT", value: "World" } ]
      setWorkingDirectoryM "/tmp"
      setAddedCapabilitiesM [ NetAdmin, SysAdmin ]
      setWaitStrategyM [ StartupTimeout 2000 ]
      setPrivilegedModeM
      setReuseM

      getContainer

  cntResult <- withContainer cnt $ \c -> do
    liftEffect $ Console.log "Launched! Now executing a command"
    execResult <- exec [ "echo", "hello", "world" ] c
    liftEffect $ Console.log $ "Executed: " <> show execResult

  liftEffect $ Console.log $ "Finished: " <> show cntResult

main :: Effect Unit
main = launchAff_ $ do
  let cnt = mkContainer "redis" # configureContainer
  monadicMain

  resFiber <- forkAff $ startContainer cnt
  liftEffect $ Console.log "Started container, waiting for launch..."
  res <- joinFiber resFiber

  case res of
    Left e -> liftEffect $ Console.log $ "Error: " <> e
    Right r -> do

      liftEffect $ do
        Console.log $ "Launched! " <> show r
        port <- getMappedPort 6379 r
        Console.log $ "Mapped port: " <> show port

      -- stay up one minute to allow debugging
      delay (Milliseconds 30000.0)

      -- restart container
      restartFiber <- forkAff $ restartContainer r
      liftEffect $ Console.log "Restarting container..."
      restartResult <- joinFiber restartFiber
      case restartResult of
        Left e -> liftEffect $ Console.log $ "Error: " <> e
        Right r' -> do
          delay (Milliseconds 10000.0)
          stopFiber <- forkAff $ stopContainer r'
          liftEffect $ Console.log "Stopping container..."
          stopResult <- joinFiber stopFiber
          case stopResult of
            Left e -> liftEffect $ Console.log $ "Error: " <> e
            Right r2 -> do
              liftEffect $ Console.log $ "Stopped! " <> show r2

  where
  configureContainer :: TestContainer -> TestContainer
  configureContainer =
    setExposedPorts [ 6379 ]
      >>> setPullPolicy AlwaysPull
      >>> setEnvironment [ { key: "SOMETHING", value: "ELSE" } ]
      >>> setLabels [ { key: "mylabel", value: "myvalue" }, { key: "secondlabel", value: "secondvalue" } ]
      >>> setBindMounts [ { source: "flake.lock", target: "/flake.lock", readOnly: true } ]
      >>> setName "redis-container"
      >>> setCopyFilesToContainer
        [ (FromSource "package.json" "/etc/package.json")
        , (FromContent "some-content" "/etc/content.txt")
        , (FromDirectory ".psci_modules" "/etc/psci_modules")
        ]
      >>> setWorkingDirectory "/data"
      >>> setDefaultLogDriver
      >>> setTmpFs { path: "/temp_redis_data", mountOptions: "rw,noexec,nosuid,size=65536k" }
      >>> setUser "redis"
      >>> setPrivilegedMode
      >>> setAddedCapabilities [ AuditRead, LinuxImmutable, SysNice ]
      >>> setDroppedCapabilities [ Kill, MkNod, Chown ]
      >>> setIpcMode None
      >>> setResourcesQuota { cpu: 0.2, memory: 1.0 }
      >>> setSharedMemorySize (512 * 1024 * 1024)
      >>> setReuse
      >>> setWaitStrategy [ LogOutput "Ready to accept connections tcp" 1 ]
