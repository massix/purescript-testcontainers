# Testcontainers for PureScript

![Tests](https://github.com/massix/purescript-testcontainers/actions/workflows/purescript.yml/badge.svg)

![Testcontainers Logo](https://testcontainers.com/images/testcontainers-logo.svg)

## Table of contents

- [Introduction](#introduction)
- [Quick example](#quick-example)
- [Features](#features)
- [Local Development](#local-development)
- [Containers](#containers)
  - [Create container](#create-container)
  - [Start a container](#start-a-container)
  - [Stop a container](#stop-a-container)
  - [Set a Wait Strategy](#set-a-wait-strategy)
  - [Environment variables](#environment-variables)
  - [Privileged Mode](#privileged-mode)
  - [Capabilities](#capabilities)
  - [Set User](#set-user)
  - [Exec a command](#exec-a-command)
  - [Use the withContainer helper](#use-the-withcontainer-helper)
  - [Configure with a Monad](#configure-with-a-monad)
- [Network](#network)
  - [Create a network](#create-a-network)
  - [Attach a container to a network](#attach-a-container-to-a-network)
- [Docker Compose](#docker-compose)

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
```

This is to be considered as a _low-level_ library, hence it is not making use of
complex monads transformers or anything, it's up to the users to define their own
mtl stack if they need to.

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
If you want to test the library locally, you just need to have the latest
versions of `spago` and `purs` installed.

### Nix
For NixOS and nix users, a [flake.nix](./flake.nix) is provided, it uses the
`purescript-overlay` to install the latest versions of `spago` and `purs`. If
you use `direnv` you can simply `direnv allow .` to start a local development
shell.

## Containers

### Create container

### Start a container

### Stop a container

### Set a Wait Strategy

### Environment variables

### Privileged Mode

### Capabilities

### Set User

### Exec a command

### Use the `withContainer` helper

### Configure with a Monad

## Network

### Create a network

### Attach a container to a network

## Docker Compose

### Create an environment

### Up an environment

### Down an environment

### Use the `withCompose` helper

### Get a container from the environment

### Set Wait strategies for containers

### Use Profiles

### Automatically rebuild

### Use Environment variables

#### From files

#### From Code

