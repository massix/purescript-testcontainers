module Test.Ports where

import Prelude

import Data.Either (isRight)
import Partial.Unsafe (unsafePartial)
import Test.Partials (forceRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual, shouldSatisfy)
import Test.TestContainers (getFirstMappedPort, getMappedPort, mkContainer, withContainer)
import Test.TestContainers.Monad (configure, getContainer, setExposedPortsM, setPullPolicyM, setReuseM, setUserM, setWaitStrategyM)
import Test.TestContainers.Types (PullPolicy(..), WaitStrategy(..))
import Test.Utils (launchCommand)

portMappingTest :: Spec Unit
portMappingTest = do
  describe "Port Mappings" $ do
    it "should map ports" $ do
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

    it "should map multiple ports" $ do
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

