# Purescript-TestContainers

![Tests](https://github.com/massix/purescript-testcontainers/actions/workflows/purescript.yml/badge.svg)

## Description
This is my first attempt at creating a library for PureScript.

It is still a work in progress, but some functionalities are there already, it might
cover some of the use-cases (creating a container, executing commands, opening ports)
when integration testing, for example creating a PostgreSQL container and connecting
to it.

The [test] folder contains some very basic examples.


## Usage
In order to use this library you need to install `testcontainers` using node in your
environment:

    $ npm install testcontainers

After that, you need to install this library in your project, it can be done quite
easily if using `spago@next`, simply modify the workspace section of the `spago.yaml`
file and add this GitHub repository as a remote source.

If you're using older versions of spago, it might be more difficult and I honestly
do not know how to do it (PRs are welcome!).

## Basic Examples

### Start a PostgreSQL container and connect to it using

```purescript
module TestPostgreSQL where
import Prelude
import Effect (Effect)
import Test.TestContainers as TC
import Test.TestContainers.Types as TC
import Yoga.Postgres as YP

main :: Effect Unit
main = do
  TC.withContainer mkPostgreSQLContainer $ \container -> do
    liftEffect $ "PostgreSQL container started: " <> show container
    { host, port } <- liftEffect $ do
      host <- TC.getHost container
      port <- TC.getFirstMappedPort container
      pure { host, port }

    pool <- YG.mkPool $ mkConnection host port

    launchAff_ $ do
      withClient pool $ \c -> do
        YG.execute_ (YG.Query "create table t (id int)") c

  where
  mkPostgreSQLContainer :: TC.TestContainer
  mkPostgreSQLContainer =
    TC.mkContainer "postgres:latest" $
      TC.setExposedPorts [ 5432 ] <<<

      -- PostgreSQL container will output this string twice while starting
      -- The first one is before creating the database, the second one is the real one
      TC.setWaitStrategy [ (TC.LogOutput "database system is ready to accept connections" 2) ] <<<
      TC.setEnvironment
        [ { key: "POSTGRES_DB", value: "test" }
        , { key: "POSTGRES_USER", value: "test" }
        , { key: "POSTGRES_PASSWORD", value: "test" }
        ]

  mkConnection :: String -> Int -> YG.ConnectionInfo
  mkConnection host port = YG.connectionInfoFromString $ "postgres://" <> host <> ":" <> show port <> "/test"
```


## Contributions

Contributions are welcome, the project is still fresh and there's a lot to do!


## Features

- [x] Basic containers management (start, stop, port forwarding, ...)
- [x] Network management (create, attach, delete)
- [x] Docker compose integration
- [ ] Building of images

