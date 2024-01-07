module Test.NetworkMode (networkModeTest) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (error, throwError)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions.String (shouldContain)
import Test.Testcontainers (setCommand, setNetworkMode, withContainer)
import Test.Testcontainers.Types (NetworkMode(..))
import Test.Utils (mkAffContainer)

networkModeTest :: Spec Unit
networkModeTest = describe "Network Mode" $ do
  it "should set bridge network mode" $ do
    cnt <- mkAffContainer "alpine:latest" $
      setNetworkMode (NetworkMode "bridge")
        <<< setCommand [ "sleep", "infinity" ]

    res <- withContainer cnt $ \_ -> do
      pure unit

    case res of
      Left e -> throwError $ error e
      Right _ -> pure unit

  it "should set host network mode" $ do
    cnt <- mkAffContainer "alpine:latest" $
      setNetworkMode (NetworkMode "host")
        <<< setCommand [ "sleep", "infinity" ]

    res <- withContainer cnt $ \_ -> do
      pure unit

    case res of
      Left e -> throwError $ error e
      Right _ -> pure unit

  it "should detect invalid network modes" $ do
    cnt <- mkAffContainer "alpine:latest" $
      setNetworkMode (NetworkMode "invalid")
        <<< setCommand [ "sleep", "infinity" ]

    -- We should not enter here
    res <- withContainer cnt $ \_ -> do
      pure unit

    case res of
      Left e -> e `shouldContain` "network invalid not found"
      Right _ -> throwError $ error "invalid network mode not detected"
