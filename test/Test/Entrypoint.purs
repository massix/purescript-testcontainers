module Test.Entrypoint where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (error, throwError)
import Test.Assertions (shouldInclude)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Testcontainers (setCommand, setCopyFilesToContainer, setEntrypoint, withContainer)
import Test.Testcontainers.Types (CopyContentToContainer(..), FileMode(..))
import Test.Utils (launchCommand, mkAffContainer)

entrypointTest :: Spec Unit
entrypointTest = describe "Test Entrypoint" $ do
  it "should override the entrypoint" $ do
    sleeper <- mkAffContainer "alpine:latest" $
      setEntrypoint [ "/docker-entrypoint.sh" ]
        <<< setCommand [ "30" ]
        <<< setCopyFilesToContainer [ FromSource "./test/docker-entrypoint.sh" "/docker-entrypoint.sh" (FileMode "0755") ]

    res <- withContainer sleeper $ \c -> do
      launchCommand c [ "ps" ]
        (\s -> s `shouldInclude` "sleep 30")
        (\exitCode -> exitCode `shouldEqual` 0)

    case res of
      Left e -> throwError $ error e
      Right _ -> pure unit

