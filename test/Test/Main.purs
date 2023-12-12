module Test.Main (main) where

--------------------------------------------------------------------------------

import Prelude

import Data.Either (isLeft, isRight)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Test.Assertions (shouldInclude)
import Test.Binds (bindTest)
import Test.Partials (forceRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual, shouldSatisfy)
import Test.Spec.Assertions.String (shouldStartWith)
import Test.Spec.Config (Config, defaultConfig)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec')
import Test.TestContainers (getFirstMappedPort, getHost, getId, getMappedPort, getName, mkContainer, withContainer)
import Test.TestContainers.Monad (configure, getContainer, setCommandM, setExposedPortsM, setNameM, setPrivilegedModeM, setPullPolicyM, setReuseM, setUserM, setWaitStrategyM)
import Test.TestContainers.Types (PullPolicy(..), WaitStrategy(..))
import Test.Utils (launchCommand)

--------------------------------------------------------------------------------

basicTest :: Spec Unit
basicTest = do
  describe "Basic commands" $ do
    it "should launch a basic container" $ do
      let
        cnt = mkContainer "alpine:latest" # configure $ do
          setCommandM [ "sleep", "360" ]
          setPullPolicyM AlwaysPull
          setNameM "sleeper" -- do not do this in production
          setPrivilegedModeM
          ret <- getContainer
          pure ret

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

portMappingTest :: Spec Unit
portMappingTest = do
  describe "Port Mappings" $ do
    it "should be able to map ports" $ do
      let
        nginx = mkContainer "nginx:alpine" # configure $ do
          setExposedPortsM [ 80 ]
          setPullPolicyM AlwaysPull
          setReuseM
          ret <- getContainer
          pure ret

      void $ withContainer nginx $ \c -> do
        ports <- getMappedPort 80 c
        ports `shouldSatisfy` isRight
        let mappedPort = unsafePartial $ forceRight ports
        mappedPort `shouldSatisfy` (\p -> p > 0)

        singlePort <- getFirstMappedPort c
        singlePort `shouldSatisfy` isRight
        let singleMappedPort = unsafePartial $ forceRight singlePort
        singleMappedPort `shouldEqual` mappedPort

        launchCommand c [ "whoami" ]
          (\s -> s `shouldEqual` "root\n")
          (\code -> code `shouldEqual` 0)

    it "should be able to map multiple ports" $ do
      let
        redis = mkContainer "redis:latest" # configure $ do
          setExposedPortsM [ 6379, 6270 ]
          setPullPolicyM AlwaysPull
          setWaitStrategyM [ LogOutput "Ready to accept connections tcp" 1 ]
          setUserM "redis"
          ret <- getContainer
          pure ret

      void $ withContainer redis $ \c -> do
        port <- getMappedPort 6379 c
        port `shouldSatisfy` isRight
        let mappedPort = unsafePartial $ forceRight port

        port' <- getMappedPort 6270 c
        port' `shouldSatisfy` isRight
        let mappedPort' = unsafePartial $ forceRight port'

        mappedPort `shouldNotEqual` mappedPort'

        launchCommand c [ "whoami" ]
          (\s -> s `shouldEqual` "redis\n")
          (\_ -> pure unit)

main :: Effect Unit
main = launchAff_ $ runSpec' configuration [ consoleReporter ] $ do
  describe "Containers" $ do
    basicTest
    portMappingTest
    bindTest

  where
  configuration :: Config
  configuration = defaultConfig { timeout = Just $ Milliseconds 120_000.0, failFast = true }
