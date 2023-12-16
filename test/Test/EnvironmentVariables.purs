module Test.EnvironmentVariables where

import Prelude

import Data.Either (Either(..), isRight)
import Effect.Aff (error, throwError)
import Partial.Unsafe (unsafePartial)
import Test.Assertions (shouldInclude)
import Test.Partials (forceRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.TestContainers (exec, setCommand, setEnvironment, withContainer)
import Test.Utils (mkAffContainer)

environmentTest :: Spec Unit
environmentTest = describe "Environment Variables" $ do
  it "should set environment variables properly" $ do
    sleeperContainer <- mkAffContainer "alpine:latest" $ setEnvironment env <<< setCommand [ "sleep", "360" ]
    res <- withContainer sleeperContainer $ \c -> do
      execResult <- exec [ "env" ] c
      execResult `shouldSatisfy` isRight

      let { output, exitCode } = unsafePartial $ forceRight execResult
      output `shouldInclude` "SOME_VARIABLE=SOME_VALUE"
      output `shouldInclude` "OTHER_VARIABLE=OTHER_VALUE"
      exitCode `shouldEqual` 0

    case res of
      Left e -> throwError $ error e
      Right _ -> pure unit

  where
  env =
    [ { key: "SOME_VARIABLE", value: "SOME_VALUE" }
    , { key: "OTHER_VARIABLE", value: "OTHER_VALUE" }
    ]

