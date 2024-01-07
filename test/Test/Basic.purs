module Test.Basic where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), isLeft, isRight)
import Data.String (trim)
import Effect.Aff (error)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Test.Assertions (shouldInclude)
import Test.Partials (forceRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual, shouldSatisfy)
import Test.Spec.Assertions.String (shouldStartWith)
import Test.Testcontainers (getFirstMappedPort, getHost, getId, getMappedPort, getName, setCommand, setName, setPrivilegedMode, setPullPolicy, setUser, setWorkingDirectory, withContainer)
import Test.Testcontainers.Types (PullPolicy(..))
import Test.Utils (launchCommand, mkAffContainer)

basicTest :: Spec Unit
basicTest = do
  describe "Basic stuff" $ do
    it "should launch a basic container" $ do
      cnt <- mkAffContainer "alpine:latest" $
        setCommand [ "sleep", "360" ]
          <<< setPullPolicy AlwaysPull
          <<< setName "sleeper"
          <<< setPrivilegedMode

      void $ withContainer cnt $ \c -> do
        containerIdE <- liftEffect $ getId c
        containerIdE `shouldSatisfy` isRight

        let containerId = unsafePartial $ forceRight containerIdE
        containerId `shouldNotEqual` ""

        launchCommand c [ "echo", "hello", "world" ]
          (\s -> s `shouldEqual` "hello world\n")
          (\code -> code `shouldEqual` 0)
        launchCommand c [ "whoami" ]
          (\s -> s `shouldEqual` "root\n")
          (\code -> code `shouldEqual` 0)
        launchCommand c [ "ps", "x" ]
          (\s -> s `shouldInclude` "sleep 360")
          (\_ -> pure unit)
        launchCommand c [ "ping", "-c", "1", "127.0.0.1" ]
          (\s -> s `shouldInclude` "ping statistics")
          (\code -> code `shouldEqual` 0)
        launchCommand c [ "hostname" ]
          (\s -> containerId `shouldStartWith` trim s)
          (\_ -> pure unit)

        eitherName <- liftEffect $ getName c
        eitherName `shouldSatisfy` isRight

        eitherHost <- liftEffect $ getHost c
        eitherHost `shouldSatisfy` isRight

        let name = unsafePartial $ forceRight eitherName
        name `shouldEqual` "/sleeper"

        let host = unsafePartial $ forceRight eitherHost
        host `shouldEqual` "localhost"

        -- Since there are no mapped ports, both these functions should fail
        p <- getMappedPort 8080 c
        p `shouldSatisfy` isLeft

        p' <- getFirstMappedPort c
        p' `shouldSatisfy` isLeft

    it "should change the user" $ do
      -- default user for redis container is "root"
      redis <- mkAffContainer "redis:latest" $ setUser "redis"

      res <- withContainer redis $ \c -> do
        launchCommand c [ "whoami" ]
          (\s -> s `shouldEqual` "redis\n")
          (\_ -> pure unit)

      case res of
        Left e -> throwError $ error e
        Right _ -> pure unit

    it "should change the working directory" $ do
      sleeper <- mkAffContainer "alpine:latest" $
        setWorkingDirectory "/tmp"
          <<< setCommand [ "sleep", "30" ]

      res <- withContainer sleeper $ \c -> do
        launchCommand c [ "pwd" ]
          (\s -> s `shouldEqual` "/tmp\n")
          (\_ -> pure unit)

      case res of
        Left e -> throwError $ error e
        Right _ -> pure unit

