module Test.TestContainers.Types
  ( KV
  , BindMounts
  , FilePath
  , TmpFS
  , User
  , ResourcesQuota
  , MemorySize
  , ExecResult
  , GenericContainer
  , StartedTestContainer
  , StoppedTestContainer
  , StartedNetwork
  , StoppedNetwork
  , GenericNetwork
  , NetworkId
  , NetworkName
  , ExtraHost
  , Capability(..)
  , IPCMode(..)
  , CopyContentToContainer(..)
  , TestContainer(..)
  , Image(..)
  , WaitStrategy(..)
  , class IsImage
  , toImage
  , PullPolicy(..)
  , capToString
  , StartupTimeout(..)
  , FileMode(..)
  , NetworkMode(..)
  , Network(..)
  ) where

import Prelude

newtype Image = Image String

class IsImage c where
  toImage :: c -> Image

instance IsImage Image where
  toImage = identity

instance IsImage String where
  toImage = Image

derive instance Eq Image

type KV = { key :: String, value :: String }
type BindMounts = { source :: FilePath, target :: FilePath, readOnly :: Boolean }
type FilePath = String
type TmpFS = { path :: FilePath, mountOptions :: String }
type User = String
type ResourcesQuota = { cpu :: Number, memory :: Number }
type MemorySize = Int
type ExecResult = { output :: String, exitCode :: Int }
type NetworkId = String
type NetworkName = String
type ExtraHost = { host :: String, ipAddress :: String }

newtype StartupTimeout = StartupTimeout Int
newtype FileMode = FileMode String
newtype NetworkMode = NetworkMode String

foreign import data GenericContainer :: Type
foreign import data StartedTestContainer :: Type
foreign import data StoppedTestContainer :: Type

foreign import data GenericNetwork :: Type
foreign import data StoppedNetwork :: Type
foreign import data StartedNetwork :: Type

data Capability
  = AuditControl
  | AuditRead
  | AuditWrite
  | BlockSuspend
  | BPF
  | CheckpointRestore
  | Chown
  | DACOverride
  | DACReadSearch
  | FOwner
  | FSetID
  | IPCLock
  | IPCOwner
  | Kill
  | Lease
  | LinuxImmutable
  | MACAdmin
  | MACOverride
  | MkNod
  | NetAdmin
  | NetBindService
  | NetBroadcast
  | NetRaw
  | PerfMon
  | SetGid
  | SetFCap
  | SetPCap
  | SetUid
  | SysAdmin
  | SysBoot
  | SysChroot
  | SysModule
  | SysNice
  | SysPAcct
  | SysPTrace
  | SysRawIO
  | SysResource
  | SysTime
  | SysTtyConfig
  | WakeAlarm

capToString :: Capability -> String
capToString = case _ of
  AuditControl -> "AUDIT_CONTROL"
  AuditRead -> "AUDIT_READ"
  AuditWrite -> "AUDIT_WRITE"
  BlockSuspend -> "BLOCK_SUSPEND"
  BPF -> "BPF"
  CheckpointRestore -> "CHECKPOINT_RESTORE"
  Chown -> "CHOWN"
  DACOverride -> "DAC_OVERRIDE"
  DACReadSearch -> "DAC_READ_SEARCH"
  FOwner -> "FOWNER"
  FSetID -> "FSETID"
  IPCLock -> "IPC_LOCK"
  IPCOwner -> "IPC_OWNER"
  Kill -> "KILL"
  Lease -> "LEASE"
  LinuxImmutable -> "LINUX_IMMUTABLE"
  MACAdmin -> "MAC_ADMIN"
  MACOverride -> "MAC_OVERRIDE"
  MkNod -> "MKNOD"
  NetAdmin -> "NET_ADMIN"
  NetBindService -> "NET_BIND_SERVICE"
  NetBroadcast -> "NET_BROADCAST"
  NetRaw -> "NET_RAW"
  PerfMon -> "PERFMON"
  SetGid -> "SETGID"
  SetFCap -> "SETFCAP"
  SetPCap -> "SETPCAP"
  SetUid -> "SETUID"
  SysAdmin -> "SYS_ADMIN"
  SysBoot -> "SYS_BOOT"
  SysChroot -> "SYS_CHROOT"
  SysModule -> "SYS_MODULE"
  SysNice -> "SYS_NICE"
  SysPAcct -> "SYS_PACCT"
  SysPTrace -> "SYS_PTRACE"
  SysRawIO -> "SYS_RAWIO"
  SysResource -> "SYS_RESOURCE"
  SysTime -> "SYS_TIME"
  SysTtyConfig -> "SYS_TTY_CONFIG"
  WakeAlarm -> "WAKE_ALARM"

instance Show Capability where
  show = capToString

data IPCMode
  = Default
  | None
  | Private
  | Shareable
  | WithContainer String
  | Host

ipcModeToString :: IPCMode -> String
ipcModeToString = case _ of
  Default -> ""
  None -> "none"
  Private -> "private"
  Shareable -> "shareable"
  WithContainer s -> "container:" <> s
  Host -> "host"

instance Show IPCMode where
  show = ipcModeToString

data CopyContentToContainer
  = FromSource FilePath FilePath FileMode
  | FromContent String FilePath FileMode
  | FromDirectory FilePath FilePath FileMode

data TestContainer
  = StartedTestContainer Image StartedTestContainer
  | StoppedTestContainer Image StoppedTestContainer
  | GenericContainer Image GenericContainer

data Network
  = StartedNetwork StartedNetwork
  | StoppedNetwork StoppedNetwork
  | GenericNetwork GenericNetwork

instance Show Network where
  show (StartedNetwork _) = "(StartedNetwork)"
  show (StoppedNetwork _) = "(StoppedNetwork)"
  show (GenericNetwork _) = "(GenericNetwork)"

data PullPolicy = AlwaysPull | DefaultPolicy

instance Show TestContainer where
  show (StartedTestContainer (Image s) _) = "(StartedTestContainer: " <> s <> ")"
  show (StoppedTestContainer (Image s) _) = "(StoppedTestContainer: " <> s <> ")"
  show (GenericContainer (Image s) _) = "(GenericContainer: " <> s <> ")"

instance Eq TestContainer where
  eq (StartedTestContainer i _) (StartedTestContainer i' _) = i == i'
  eq (StoppedTestContainer i _) (StoppedTestContainer i' _) = i == i'
  eq (GenericContainer i _) (GenericContainer i' _) = i == i'
  eq _ _ = false

-- | Define the Wait Strategy
data WaitStrategy
  = ListeningPorts -- ^ Default waiting strategy, wait for the exposed ports to be available
  | LogOutput String Int -- ^ Look in the logs for the provided String to appear at least Int times
  | HealthCheck -- ^ Wait until the health check is healthy
  | HttpStatusCode String Int Int -- ^ Wait until the Http request at the path String and port Int returns the statuscode Int
  | HttpResponsePredicate String Int (String -> Boolean) -- ^ Similar to above, but instead of the statuscode, a predicate is required
  | ShellCommand String -- ^ Run the provided shell command and wait until it returns exit code 0
