module Test.EnvironmentVariables where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (error, throwError)
import Test.Assertions (shouldInclude)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.TestContainers (setCommand, setEnvironment, withContainer)
import Test.Utils (launchCommand, mkAffContainer)

environmentTest :: Spec Unit
environmentTest = describe "Environment Variables" $ do
  it "should set environment variables properly" $ do
    sleeperContainer <- mkAffContainer "alpine:latest" $
      setEnvironment env
        <<< setCommand [ "sleep", "360" ]

    res <- withContainer sleeperContainer $ \c -> do
      launchCommand c [ "env" ]
        ( \s -> do
            s `shouldInclude` "SOME_VARIABLE=SOME_VALUE"
            s `shouldInclude` "OTHER_VARIABLE=OTHER_VALUE"
        )
        (\exitCode -> exitCode `shouldEqual` 0)

    case res of
      Left e -> throwError $ error e
      Right _ -> pure unit

  where
  env =
    [ { key: "SOME_VARIABLE", value: "SOME_VALUE" }
    , { key: "OTHER_VARIABLE", value: "OTHER_VALUE" }
    ]

