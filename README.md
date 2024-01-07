# Testcontainers for PureScript

![Tests](https://github.com/massix/purescript-testcontainers/actions/workflows/purescript.yml/badge.svg)

![Testcontainers Logo](https://testcontainers.com/images/testcontainers-logo.svg)

## Table of contents

- [Introduction](#introduction)
- [Quick example](#quick-example)
- [Features](#features)
- [Local Development](#local-development)
  - [Nix](#nix)
- [Containers](#containers)
  - [Create container](#create-container)
  - [Start a container](#start-a-container)
  - [Stop a container](#stop-a-container)
  - [Configuring the container](#configuring-the-container)
    - [Port Mapping](#port-mapping)
    - [Set a Wait Strategy](#set-a-wait-strategy)
      - [Configuring the timeout](#configuring-the-timeout)
      - [ListeningPorts](#listeningports)
      - [LogOutput](#logoutput)
      - [HealthCheck](#healthcheck)
      - [HttpStatusCode](#httpstatuscode)
      - [HttpResponsePredicate](#httpresponsepredicate)
      - [ShellCommand](#shellcommand)
    - [Environment variables](#environment-variables)
    - [Privileged Mode](#privileged-mode)
    - [Capabilities](#capabilities)
    - [Set User](#set-user)
    - [Set Command](#set-command)
    - [Exec a command](#exec-a-command)
    - [Use the withContainer helper](#use-the-withcontainer-helper)
- [Network](#network)
  - [Create a network](#create-a-network)
  - [Start a network](#start-a-network)
  - [Attach a container to a network](#attach-a-container-to-a-network)
- [Docker Compose](#docker-compose)
  - [Create an environment](#create-an-environment)
  - [Up an environment](#up-an-environment)
  - [Down an environment](#down-an-environment)
  - [Use the `withCompose` helper](#use-the-withcompose-helper)
  - [Get a container from the environment](#get-a-container-from-the-environment)
  - [Set Wait strategies for containers](#set-wait-strategies-for-containers)
  - [Use Profiles](#use-profiles)
  - [Automatically rebuild](#automatically-rebuild)
  - [Use Environment variables](#use-environment-variables)
    - [From files](#from-files)
    - [From Code](#from-code)

## Introduction
[Testcontainers](https://testcontainers.com/) is an **opensource framework**
for providing throwaway, lightweight instances of databases, message brokers,
web browsers, or just about **anything that can run in a Docker container**.

No more need for mocks or complicated environment configurations.
**Define your test dependencies as code**, then simply run your tests and
containers will be created and then deleted.

With support for many languages and testing frameworks,
**all you need is Docker**.

## Quick example
A lot of examples are in the [tests folder](./test/Test/), but since we all
love to see some code from time to time, here is a very quick example on how to
launch an `alpine` container and execute the `ps` command in it, checking the
result:

```purescript
module Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console as Console
import Test.TestContainers as TC

main :: Effect Unit
main = do
  launchAff_ $ do
    let alpineContainer = TC.setCommand [ "sleep", "infinity" ] $ TC.mkContainer "alpine"
    eitherStarted <- TC.startContainer alpineContainer
    case eitherStarted of
      Left err -> Console.logShow err
      Right started -> do
        eitherExec <- TC.exec [ "ps" ] started
        case eitherExec of
          Left err -> Console.logShow err
          Right { output, exitCode } -> do
            Console.log $ "ps output: " <> output <> ", exitCode: " <> show exitCode

        void $ TC.stopContainer started
```

To avoid some of the `case _ of` a couple of common wrappers are provided. The
code above can be rewritten as follows:

```purescript
module Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console as Console
import Test.TestContainers as TC

main :: Effect Unit
main = do
  launchAff_ $ do
    let alpineContainer = TC.setCommand [ "sleep", "infinity" ] $ TC.mkContainer "alpine"
    void $ TC.withContainer alpineContainer $ \started -> do
      eitherExec <- TC.exec [ "ps" ] started
      case eitherExec of
        Left err -> Console.logShow err
        Right { output, exitCode } -> do
          Console.log $ "ps output: " <> output <> ", exitCode: " <> show exitCode
```

This is to be considered as a _low-level_ library, hence it is not making use of
complex monads transformers or anything, it's up to the users to define their own
`mtl` stack if they need to.

The library uses `Either String a` as a generic return value for almost all the
operations, where `a` is the success type, depending on the function called,
and `String` is to return the errors coming from the underlying FFI interface.
## Features
Not all the features of the original Testcontainers library have been
implemented yet, I plan to cover 100% of the functionalities but it will take
some time to develop everything.

For now, this is a list of the supported features, more details for each
feature are provided further down in the document.

- [X] Creation of containers
- [X] Basic handling (start/stop) of containers
- [X] Define wait strategies
- [X] Start in privileged mode
- [X] Start with capabilities (enable or disabled)
- [X] Handle users
- [X] Launch commands inside of containers
- [X] Create a network
- [X] Attach containers to a network
- [X] Use `docker-compose` definitions
- [X] Up and down of a compose environment
- [X] Get containers running in a compose environment
- [X] Start containers based on a profile
- [X] Rebuild containers automatically
- [X] Set environment variables from files

## Local Development
If you want to test the library locally, you need to have:
- the latest version of `spago` and `purs`
- a running `docker` daemon
- the `testcontainers` package installed from `npm`

After cloning the repository locally, you can run the tests via
`spago` by issuing the command: `spago test`.

### Nix
For NixOS and nix users, a [flake.nix](./flake.nix) is provided, it uses the
`purescript-overlay` from `thomashoneyman` to install the latest versions of
`spago` and `purs`. If you use `direnv` you can simply `direnv allow .` to
start a local development shell.

You will still need to install the `testcontainers` package from `npm` in your
local environment on your own. Usually it is just a matter of running `npm
install`

**Warning**: since I only use a GNU/Linux environment, the `flake.nix` is
configured only for `x86_64-linux` architecture. If you work on a different
architecture or environment, feel free to modify the `flake.nix` file and send
me a Pull Request.

## Containers
The most basic building block of the library is the `container` entity. It
allows users to create, start, interact and stop containers. It is most
probably the entity you will use the most in your projects and it is the most
complete one for this wrapper. A container is defined with the following
union data:

```purescript
data TestContainer
  = StartedTestContainer Image StartedTestContainer
  | StoppedTestContainer Image StoppedTestContainer
  | GenericContainer Image GenericContainer
```

Where `Image` is a `newtype` for a `String` object, defining a complete
reference to a docker image (e.g. `redis:latest`).

For convenience, a `Typeclass` is defined, which allows `String`s to be
converted easily and used in place of the `newtype`:

```purescript
newtype Image = Image String

class IsImage c where
  toImage :: c -> Image

instance IsImage Image where
  toImage = identity

instance IsImage String where
  toImage = Image
```

Although the constructors for the `TestContainer` data are public, you are
**strongly advised** to refrain from using them directly and instead use the
provided _smart constructor_ `mkContainer`, which has the following signature:

```purescript
mkContainer :: ∀ a. IsImage a => a -> TestContainer
```

The reason why, is because the underlying definition uses some `foreign data`
which represent *stateful* JavaScript objects used by the `Testcontainers`
library directly. All the intricacies of the original library are handled
"transparently" by the wrapper.

### Create container
As stated above, creating a container is as simple as calling the `mkContainer`
function, providing a valid definition of a docker image. The definition can
either be a local image or a remote one, it will use the docker daemon to solve
the reference, so by default (and in most standard installations of docker) it
will search for the image on `docker.io` hub.

#### Example
```purescript
-- Create a container for postgresql, version 14 and the alpine variant
mkContainer (Image "postgres:14-alpine")

-- Since the 'mkContainer' function expects a typeclass IsImage, the same
-- can be written as follows
mkContainer "postgres:14-alpine"
```


### Start a container
Once you have your container, you can start it by calling the `startContainer`
function, for which the signature is the following:
```purescript
startContainer :: ∀ m. MonadAff m => TestContainer -> m (Either String TestContainer)
```

This function has some **side effects**, hence it expects to be run inside of
an asynchronous monad (the `Aff` monad). It will return an `Either` value,
containing either an error message - already converted to a string - or a
`StartedTestContainer` value.

#### Example
This is the most basic example of starting a container inside of the `Aff`
monad itself.
```purescript
testContainer :: Aff Unit
testContainer = do
  started <- startContainer $ mkContainer "redis:latest"
  -- do something with the container
  void $ stopContainer started
```

You can also use the `Effect` monad by launching an `Aff` computation inside:
```purescript
testContainer :: Effect Unit
testContainer = launchAff_ $
    void $ stopContainer (startContainer $ mkContainer "redis:latest")
```


### Stop a container
Similar to while starting a container, you can stop it by calling the
`stopContainer` function, which has the following signature:
```purescript
stopContainer :: ∀ m. MonadAff m => TestContainer -> m (Either String TestContainer)
```

Just like the `startContainer` function, this function has **side-effects**,
hence it expects to be run inside of an asynchronous monad.

### Configuring the container
For configuring the containers I wanted to provide a similar experience to what
is provided by the original library, for this reason all the functions which
interact with the setup of the container share the same signature:

```purescript
configureSomething :: SomeParameter -> TestContainer -> TestContainer
```

This will allow you to take advantage of the __partial application__, one of the
main advantages of curried functions in functional programming and compose all
the configuration functions together, for example:

```purescript
configureContainer :: TestContainer -> TestContainer
configureContainer =
  setEnvironment env
    <<< setUser "root"
    <<< setThis "that" "andThat"
    <<< setWaitStrategy [ SomeWaitStrategy ]

main :: Effect Unit
main =
  let 
    container = configureContainer $ mkContainer "redis:latest"
  in do
    startContainer container
    --- etc
```

#### Port Mapping
Most of the times, when you want to test something with docker is a service
which will be exposed via some standard TCP or UDP ports. The most basic
configuration function is then to `map` those ports to a localhost port, in
order to be able to access the exposed service from the host machine.

The signature of the function is:
```purescript
setExposedPorts :: Array Int -> TestContainer -> TestContainer
```

**Warning**: by default, `Testcontainers` will not expose the same port to the
host, this is to make it possible to run multiple tests in parallel, once you
have set the `exposedPorts` you will need to retrieve the real ports exposed
using one of the `getMappedPort` functions:
```purescript
getMappedPort :: ∀ m. MonadEffect m => Int -> TestContainer -> m (Either String Int)
getFirstMappedPort :: ∀ m. MonadEffect m => TestContainer -> m (Either String Int)
```

These functions only work on **started containers**.

##### Example
```purescript
testRedis :: Aff Unit
testRedis = do
  started <- startContainer $ setExposedPorts [ 6379 ] $ mkContainer "redis:latest"
  exposedPort <- liftEffect $ getMappedPort 6379 started
  -- now do something with the exposed port
```

When a container only exposes a single port, as per the example above, you can
use the `getFirstMappedPort` function:
```purescript
testRedis :: Aff Unit
testRedis = do
  started <- startContainer $ setExposedPorts [ 6379 ] $ mkContainer "redis:latest"
  exposedPort <- liftEffect $ getFirstMappedPort started
  -- now do something with the exposed port
```


#### Set a Wait Strategy
Waiting strategies are a mechanism used internally by `Testcontainers` to know
when a container is to be considered as _available_ for the rest of the code.

When setting a Wait Strategy, the library will block the execution of the code
until either the `Timeout` is reached or the Wait Strategy is satisfied.

Wait strategies are **always blocking** and you should **never bypass** them,
since some functionalities (port forwarding for example) won't be available
until the strategy is satisfied.

The signature of the function is:
```purescript
setWaitStrategy :: Array WaitStrategy -> TestContainer -> TestContainer
```

And `WaitStrategy` is a union data type that represents the different ways to
wait, almost all of the strategies defined by the original library are
implemented:

```purescript
data WaitStrategy
  = ListeningPorts -- ^ Default waiting strategy, wait for the exposed ports to be available
  | LogOutput String Int -- ^ Look in the logs for the provided String to appear at least Int times
  | HealthCheck -- ^ Wait until the health check is healthy
  | HttpStatusCode String Int Int -- ^ Wait until the Http request at the path String and port Int returns the statuscode Int
  | HttpResponsePredicate String Int (String -> Boolean) -- ^ Similar to above, but instead of the statuscode, a predicate is required
  | ShellCommand String -- ^ Run the provided shell command and wait until it returns exit code 0
```

##### Configuring the timeout
The timeout for the wait strategies listed below is configurable using the function
```purescript
newtype StartupTimeout = StartupTimeout Int
setStartupTimeout :: StartupTimeout -> TestContainer -> TestContainer
```

By default is set to 60 seconds, if the timeout is reached before the
successful completion of the defined wait strategy then the container is
immediately stopped and the `startContainer` function will return with an error
(i.e. `Left String`)

##### ListeningPorts
This is the most basic and default WaitStrategy implemented by
`Testcontainers`. When you use the `setExposedPorts` function (described
[here](#port-mapping)) it will wait until the exposed port is available. This
is done with some heuristics and while it is accurate most of the time,
sometimes it won't work properly, especially if the container is restarting
internally (for example `postgres` containers tend to start the server multiple
times while doing the initial setup). This strategy is always active and
there are no configuration parameters to configure it.

##### LogOutput
This Wait Strategy will wait for some string to appear in the container's logs
the number of times defined by the constructor.

The provided string will be treated as a **regular expression** by the
underlying library.

###### Example
```purescript
testPostgre :: Aff Unit
testPostgre = do
  let config = setExposedPorts [ 5432 ]
    <<< setWaitStrategy [ LogOutput "database system is ready to accept connections" 2 ]
      -- ^ postgresql will restart after the initial configuration, we know that the
      -- service is ready only when that line has appeared twice
  started <- startContainer $ config $ mkContainer "postgres:14-alpine"
  -- do something with postgresql container
```

##### HealthCheck
This Wait Strategy will wait until the health check defined in the `Dockerfile`
of the image is healthy. In the original library there is a way to define a
custom `HealthCheck`, but this has not been implemented in this wrapper yet.

##### HttpStatusCode
This Wait strategy will wait for a specific HttpStatusCode to be returned on
the given path and at the given port.

The constructor takes 3 parameters, the first one is the `path`, the second is
the `port` and the third is the expected `status code`.

**WARNING**: in order for this wait strategy to work, you have to
[map a port before](#port-mapping), otherwise the underlying system won't be
able to trigger the HTTP call.

###### Example
```purescript
main :: Effect Unit
main = launchAff_ $ do
  upped <- startContainer (setWaitStrategy [ HttpStatusCode "/" 80 200 ] <<< setExposedPorts [ 80 ] $ mkContainer "nginx:alpine")
  -- do something with the container
  void $ stopContainer
```

##### HttpResponsePredicate
Similar to the [HttpStatusCode](#httpstatuscode), this Wait Strategy will
interact with the underlying container via HTTP. The constructor takes 3
parameters: the `path` where to send the HTTP request to, the `port` and a
function which takes a `String` and returns a `Boolean`, the `String` is the
raw HTTP body returned by the service. The service is considered to be ready
if the predicate returns `true`.

###### Example
```purescript
main :: Aff Unit
main = do
  let config = 
    setWaitStrategy [ HttpResponsePredicate "/" 80 (\s -> "welcome to nginx" `includes` s) ]
      <<< setExposedPorts [ 80 ]

  started <- startContainer (config $ mkContainer "nginx:alpine")
  -- do something with it
  void $ stopContainer started
```

##### ShellCommand
This final constructor is `ShellCommand`, it takes a single parameter which is
a shell script to be periodically launched inside the container. It will stop
when either one of the following conditions is met:
- the timeout occurs (see [configuring the timeout](#configuring-the-timeout)
- the shell script completes successfully (i.e. its exit code is 0)

#### Environment variables
It is possible to inject environment variables inside the container using the
function:
```purescript
type KV = { key :: String, value :: String }
setEnvironment :: Array KV -> TestContainer -> TestContainer
```

The variables will be available immediately, it is the exact equivalent to the
flag `-e` of the `docker run` command.

#### Privileged Mode
If you need your container to run in `privileged` mode, you can do so with the
function
```purescript
setPrivilegedMode :: TestContainer -> TestContainer
```

Equivalent to the `--privileged` flag of `docker run`.

#### Capabilities
All the Linux capabilities can be added or removed before starting the container,
this is the list of the constructors.

**WARNING**: not all of them have been tested!

```purescript
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
```

Two functions are available to add or remove a capability:
```purescript
setAddedCapabilities :: Array Capability -> TestContainer -> TestContainer
setDroppedCapabilities :: Array Capability -> TestContainer -> TestContainer
```

#### Set User
It is also possible to change the default user of the container:
```purescript
setUser :: User -> TestContainer -> TestContainer
```
#### Set Command
It is possible to set the command to be launched upon starting the container, this
is passed to the `entrypoint` of the configured underlying container (if any)
```purescript
setCommand :: Array String -> TestContainer -> TestContainer
```

(An example is provided in the following paragraph)


#### Exec a command
Once a container is started, you can `exec` commands inside using the
following function:
```purescript
type ExecResult = { output :: String, exitCode :: Int }
exec :: ∀ m. MonadAff m => Array String -> TestContainer -> m (Either String ExecResult)
```

The `Array String` parameter is passed directly to `execve` so it has to
conform to that standard.


##### Example
```purescript
execCommandTest :: Aff Unit
execCommandTest = do
  cnt <- startContainer (setCommand [ "sleep", "infinity" ] $ mkContainer "alpine:latest")
  case cnt of
    Right c -> do
      execResultE <- exec [ "ls", "/" ] cnt
      case execResultE of
        Right { output, exitCode } -> do
          -- do something with output and exitCode
          pure unit
        Left _ -> pure unit
    Left _ -> pure unit
```


#### Use the `withContainer` helper
To make it easier to interact with containers, and to avoid the hassle of
having to either `start` and `stop` them on your own or to `bracket` somehow, a
function is provided:
```purescript
withContainer :: ∀ m a. (MonadAff m) => TestContainer -> (TestContainer -> m a) -> m (Either String a)
```

This function takes a `GenericContainer` as first parameter, a function acting
with it as the second parameter and returns an `Either` of a `String` (the
default error type) or the result of the executed action.

##### Example
```purescript
testWithContainer :: Aff Unit
testWithContainer = do
  let cnt = setCommand [ "sleep", "infinity" ] $ mkContainer "alpine:latest"
  res <- withContainer cnt $ \c -> do
    exec [ "ls", "/" ] c

  case res of
    Left e -> Console.log $ "An error occured: " <> e
    Right { output, exitCode } -> do
      Console.log $ "Exec output: " <> output <> ", exitCode: " <> show exitCode
```

The snippet above will start and stop the container automatically, after the
`exec` of the `ls /` command.


## Network
This library provides a couple of function for creating, handling and attaching
networks to containers. This will allow you to create separated services and to
allow those services to communicate with each other easily.

As usual, the [test folder](./test/Test/Network.purs) contains some integration
tests which will tell you how to use those functions.

### Create a network
Creating a network is similar to the [creation of a
container](#create-container), although the constructor is public, it is better
to use the *smart constructor* defined as follows:
```purescript
mkNetwork :: Network
```

The smart constructor takes no parameter and will create a `GenericNetwork`.

### Start a network
Once you have created your network, and before being able to attach it to
existing containers, you **must** start it using the following function:
```purescript
startNetwork :: ∀ m. MonadAff m => Network -> m (Either String Network)
```

This will return you a `StartedNetwork` or a `String` with an error message.

Please not that **the network is stopped automatically** by Testcontainers when
it is no longer used.

### Attach a container to a network
With a [`StartedNetwork`](#start-a-network), you can attach containers to it
using the following function:
```purescript
setNetwork :: Network -> TestContainer -> TestContainer
```

You can attach multiple containers to the network, it is also advised to use
the `setNetworkAliases` function in order to be able to refer to other
containers in the same network using an easy-to-remember name:
```purescript
setNetworkAliases :: Array String -> TestContainer -> TestContainer
```

#### Example
```purescript
networkTest :: Aff Unit
networkTest = do
  commonNetwork <- startNetwork mkNetwork
  case commonNetwork of
    Left e -> Console.log $ "Error: " <> e
    Right network -> do
      firstAlpine <- mkAffContainer "alpine:latest" $
        setCommand [ "sleep", "infinity" ]
          <<< setNetwork network
          <<< setNetworkAliases [ "firstAlpine" ]

      secondAlpine <- mkAffContainer "alpine:latest" $
        setCommand [ "sleep", "infinity" ]
          <<< setNetwork network
          <<< setNetworkAliases [ "secondAlpine" ]

      void $ withContainer firstAlpine $ \c ->
        void $ withContainer secondAlpine $ \c' -> do
          case (exec [ "getent", "hosts", "secondAlpine" ] c) of
            Left e -> Console.log $ "Error: " <> e
            Right { output, exitCode } -> do
              Console.log $ "Exec output: " <> output <> ", exitCode: " <> show exitCode

          case (exec [ "getent", "hosts", "firstAlpine" ] c') of
            Left e -> Console.log $ "Error: " <> e
            Right { output, exitCode } -> do
              Console.log $ "Exec output: " <> output <> ", exitCode: " <> show exitCode
  where
  mkAffContainer :: ∀ a m. IsImage a => MonadAff m => a -> (TestContainer -> TestContainer) -> m TestContainer
  mkAffContainer img conf = pure <$> conf $ mkContainer img
```

## Docker Compose
`Testcontainers` supports `docker-compose` format file and allows the creation
and handling of environments quite easily. There are some *caveats* though, for
example it will be up to the developers to expose the needed ports for the
services and to guarantee that there will be no binding conflicts. Whenever it
is possible, prefer creating your containers using the [containers'
API](#create-container) instead.

### Create an environment
Just like everything else, the constructors for the `DockerComposeEnvironment`
are open but it is better to use the *smart constructor*:
```purescript
mkComposeEnvironment :: FilePath -> (Array FilePath) -> DockerComposeEnvironment
```
The constructor takes 2 parameters, a `FilePath` pointing to the root of your
environment's context and an `Array String` of the compose files which will be
used.

### Up an environment
Once you have created your environment using the *smart constructor* described
above, you can start it easily using the function `composeUp`:
```purescript
composeUp :: ∀ m. MonadAff m => DockerComposeEnvironment -> m (Either String DockerComposeEnvironment)
```
This function returns an `Either` of a `String` describing the error or a newly
created, started docker compose environment.

### Down an environment
To stop a running environment:
```purescript
composeDown :: ∀ m. MonadAff m => DockerComposeEnvironment -> m (Either String DockerComposeEnvironment)
```
Be aware that it is **not possible** to restart a stopped environment, you will
need to create a new one.

### Use the `withCompose` helper
To avoid the hassle of having to remember to stop your environment, a
bracketing function is provided, very similar to the
[`withContainer`](#use-the-withcontainer-helper) one described a little earlier
for containers.
```purescript
withCompose :: ∀ m a. MonadAff m => DockerComposeEnvironment -> (DockerComposeEnvironment -> m a) -> m (Either String a)
```

### Get a container from the environment
Once your environment is up & running you can play with the included containers
using the provided function:
```purescript
getContainer :: ∀ m. MonadEffect m => DockerComposeEnvironment -> String -> m (Either String TestContainer)
```
The `String` parameter is the name of the service from which you want to
retrieve the container, note that if you're using `compose` version < 1.6 the
name of the service will have a `_1` suffixed while with compose >= 1.6 it will
be `-1`.

If your compose file looks like this:
```yaml
version: "3.6"
services:
  nginx:
    image: nginx:alpine
```
Then the name of the service will either be:
- `nginx_1` if you're using compose < 1.6 or
- `nginx-1` if using a more recent version of compose

To avoid having to unwrap the `Either` every time, a commodity function is available:
```purescript
withComposeContainer :: ∀ m a. MonadAff m => DockerComposeEnvironment -> String -> (TestContainer -> m a) -> m (Either String a)
```

### Set Wait strategies for containers
It is possible to define a [wait strategy](#set-a-wait-strategy) for a specific
container which is part of the Compose environment using the provided function:

```purescript
setWaitStrategy :: Array WaitStrategy -> String -> DockerComposeEnvironment -> DockerComposeEnvironment
```

The second parameter of the function is the name of the service in the compose
environment.

### Use Profiles
In the Compose specifications it is possible to define different profiles in
the same docker compose file, for example:

```yaml
version: '3.7'

services:
  postgres:
    image: postgres:14-alpine
    profiles: [ "db" ]
    environment:
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: postgres
      POSTGRES_DB: postgres
  redis:
    image: redis:alpine
    profiles: [ "cache" ]
    user: redis
  alpine:
    image: alpine:latest
    command: [ "/bin/sh", "-c", "sleep infinity" ]
    profiles: [ "backend" ]
```

Here, three profiles have been defined: **db**, **cache** and **backend**, each
profile will allow the creation of one or more service. You can specify which
profiles you want to use when creating your docker compose environment using
the provided function:

```purescript
setProfiles :: Array String -> DockerComposeEnvironment -> DockerComposeEnvironment
```

### Automatically rebuild
It is possible to tell to Testcontainers to automatically
rebuild the images needed for your services each time you
`up` your environment by using the following function:

```purescript
setRebuild :: DockerComposeEnvironment -> DockerComposeEnvironment
```

In order for this to work you need to use the `build`
definition in your compose file, just like in the example
below:

```yaml
version: '3.7'
services:
  buildedRedis:
    build:
      tags: [ "builded-redis:latest" ]
      context: .
      dockerfile: ./Dockerfile.redis
    image: builded-redis:latest
  buildedNginx:
    build:
      tags: [ "builded-nginx:latest" ]
      context: .
      dockerfile: ./Dockerfile.nginx
    ports:
      - target: 80
        published: 8080
        protocol: tcp
    image: builded-nginx:latest
```

The full example is available [in the tests folder](./test/compose/rebuildable/).

### Use Environment variables
Sometimes you want your `compose` file to be dynamically
interpreted using some environment variables, this is
possible with `Testcontainers` in two different ways.
First, let's create our dynamic compose file as follows:

```yaml
version: '3.7'
services:
  alpine:
    image: alpine:${ALPINE_TAG}
    command: ["/bin/sh", "-c", "sleep infinity"]
    environment:
      SOMEVARIABLE: "${SOMEVARIABLE}"
      QUOTEDVARIABLE: "${QUOTEDVARIABLE}"
```

For this example to work we need to provide a definition
for the three environment variables, otherwise
Testcontainers will refuse to `up` our environment.

#### From Files
The first option we have is to define our variables in a
`dotenv` file (i.e. a file where each line has the syntax
`VARIABLE=VALUE`) and then use the function below:

```purescript
setEnvironmentFile :: FilePath -> DockerComposeEnvironment -> DockerComposeEnvironment
```

Please notice that the first parameter is **relative to
the root of our environment**, so if we have defined our
environment using the following call:
```purescript
-- Remember:
-- mkComposeEnvironment :: FilePath -> (Array FilePath) -> DockerComposeEnvironment
let myEnv = mkComposeEnvironment "./test/compose/environmentfile” [ "compose.yaml" ]
```

And we want to use the `.env.custom` file located in
`./test/compose/environmentfile` all we need to do is:

```purescript
let withEnvFile = setEnvironmentFile ".env.custom" env
```

As usual, a full working example is available in the
[tests folder](./test/compose/environmentfile) of this
repository.

#### From Code
Finally, it is also possible to set the environment
variables programmatically using the following function:

```purescript
setEnvironment :: Array KV -> DockerComposeEnvironment -> DockerComposeEnvironment
```

For example, to fulfill the file described at the
beginning of this chapter we could simply:

```purescript
-- Remember:
-- mkComposeEnvironment :: FilePath -> (Array FilePath) -> DockerComposeEnvironment
let myEnv = mkComposeEnvironment "./test/compose/environmentfile” [ "compose.yaml" ]
let filledEnv =
  setEnvironment
    [ { key: "ALPINE_TAG", value: "latest" }
    , { key: "SOMEVARIABLE", value: "v" }
    , { key: "QUOTEDVARIABLE", value: "x" } ]
    myEnv
```

