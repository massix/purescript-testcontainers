module Test.Entrypoint where

import Prelude

import Data.Either (Either(..), isRight)
import Effect.Aff (error, throwError)
import Partial.Unsafe (unsafePartial)
import Test.Assertions (shouldInclude)
import Test.Partials (forceRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Test.TestContainers (exec, setCommand, setCopyFilesToContainer, setEntrypoint, withContainer)
import Test.TestContainers.Types (CopyContentToContainer(..), FileMode(..))
import Test.Utils (mkAffContainer)

entrypointTest :: Spec Unit
entrypointTest = describe "Test Entrypoint" $ do
  it "should be able to override the entrypoint" $ do
    sleeper <- mkAffContainer "alpine:latest" $
      setEntrypoint [ "/docker-entrypoint.sh" ]
      <<< setCommand ["30"]
      <<< setCopyFilesToContainer [ FromSource "./test/docker-entrypoint.sh" "/docker-entrypoint.sh" (FileMode "0755") ]

    res <- withContainer sleeper $ \c -> do
      execResult <- exec ["ps"] c
      execResult `shouldSatisfy` isRight
      let { output } = unsafePartial $ forceRight execResult

      output `shouldInclude` "sleep 30"


    case res of
      Left e -> throwError $ error e
      Right _ -> pure unit

