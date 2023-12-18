module Test.Network where

import Prelude

import Data.Either (Either(..), isRight)
import Partial.Unsafe (unsafePartial)
import Test.Assertions (shouldInclude)
import Test.Partials (forceRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Test.TestContainers (setCommand, setExtraHosts, setNetwork, setNetworkAliases, withContainer)
import Test.TestContainers.Network (getId, getName, mkNetwork, startNetwork)
import Test.Utils (launchCommand, mkAffContainer)

networkTest :: Spec Unit
networkTest = do
  describe "Network creation" $ do
    it "should be able to create a network" $ do
      startedNetwork <- startNetwork mkNetwork
      case startedNetwork of
        Left err -> fail err
        Right _ -> pure unit

    it "retrieve network information" $ do
      otherNetwork <- startNetwork mkNetwork
      otherNetwork `shouldSatisfy` isRight

      networkNameE <- getName (unsafePartial $ forceRight otherNetwork)
      networkNameE `shouldSatisfy` isRight

      networkIdE <- getId (unsafePartial $ forceRight otherNetwork)
      networkIdE `shouldSatisfy` isRight

      let realName = unsafePartial $ forceRight networkNameE
      let realId = unsafePartial $ forceRight networkIdE

      realName `shouldSatisfy` ((/=) "")
      realId `shouldSatisfy` ((/=) "")

  describe "Network utilisation" $ do
    it "should be able to define extra hosts" $ do
      alpine <- mkAffContainer "alpine:latest" $
        setCommand ["sleep", "infinity"]
        <<< setExtraHosts 
          [ { host: "foo", ipAddress: "10.250.0.42" }
          , { host: "bar", ipAddress: "10.250.0.43" }
          ]

      res <- withContainer alpine $ \c -> do
        launchCommand c ["getent", "hosts", "foo"]
          (\s -> s `shouldEqual` "10.250.0.42       foo  foo\n")
          (\exitCode -> exitCode `shouldEqual` 0)

        launchCommand c ["getent", "hosts", "bar"]
          (\s -> s `shouldEqual` "10.250.0.43       bar  bar\n")
          (\exitCode -> exitCode `shouldEqual` 0)

      case res of
        Left err -> fail err
        Right _ -> pure unit

    it "should be able to use a network" $ do
      commonNetwork <- startNetwork mkNetwork
      case commonNetwork of
        Left e -> fail e
        Right network -> do
          firstAlpine <- mkAffContainer "alpine:latest" $
            setCommand ["sleep", "infinity"]
            <<< setNetwork network
            <<< setNetworkAliases ["firstAlpine"]

          secondAlpine <- mkAffContainer "alpine:latest" $
            setCommand ["sleep", "infinity"]
            <<< setNetwork network
            <<< setNetworkAliases ["secondAlpine"]

          res <- withContainer firstAlpine $ \c ->
            withContainer secondAlpine $ \c' -> do
              launchCommand c ["getent", "hosts", "secondAlpine"]
                (\s -> s `shouldInclude` "secondAlpine  secondAlpine\n")
                (\exitCode -> exitCode `shouldEqual` 0)

              launchCommand c' ["getent", "hosts", "firstAlpine"]
                (\s -> s `shouldInclude` "firstAlpine  firstAlpine\n")
                (\exitCode -> exitCode `shouldEqual` 0)

          case res of
            Left e -> fail e
            Right _ -> pure unit
