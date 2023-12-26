module Test.Ports where

import Prelude

import Data.Either (isRight)
import Partial.Unsafe (unsafePartial)
import Test.Partials (forceRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual, shouldSatisfy)
import Test.TestContainers (getFirstMappedPort, getMappedPort, setExposedPorts, setPullPolicy, setReuse, setWaitStrategy, withContainer)
import Test.TestContainers.Types (PullPolicy(..), WaitStrategy(..))
import Test.Utils (mkAffContainer)

portMappingTest :: Spec Unit
portMappingTest = do
  describe "Port Mappings" $ do
    it "should map ports" $ do
      nginx <- mkAffContainer "nginx:alpine" $
        setExposedPorts [ 80 ]
          <<< setPullPolicy AlwaysPull
          <<< setReuse

      void $ withContainer nginx $ \c -> do
        ports <- getMappedPort 80 c
        ports `shouldSatisfy` isRight
        let mappedPort = unsafePartial $ forceRight ports
        mappedPort `shouldSatisfy` (\p -> p > 0)

        singlePort <- getFirstMappedPort c
        singlePort `shouldSatisfy` isRight
        let singleMappedPort = unsafePartial $ forceRight singlePort
        singleMappedPort `shouldEqual` mappedPort

    it "should map multiple ports" $ do
      redis <- mkAffContainer "redis:latest" $
        setExposedPorts [ 6379, 6270 ]
          <<< setPullPolicy AlwaysPull
          <<< setWaitStrategy [ LogOutput "Ready to accept connections tcp" 1 ]

      void $ withContainer redis $ \c -> do
        port <- getMappedPort 6379 c
        port `shouldSatisfy` isRight
        let mappedPort = unsafePartial $ forceRight port

        port' <- getMappedPort 6270 c
        port' `shouldSatisfy` isRight
        let mappedPort' = unsafePartial $ forceRight port'

        mappedPort `shouldNotEqual` mappedPort'

