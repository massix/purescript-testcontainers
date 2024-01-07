module Test.Privileged where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Effect.Aff (error)
import Test.Assertions (shouldInclude)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Testcontainers (setPrivilegedMode, setWaitStrategy, withContainer)
import Test.Testcontainers.Types (WaitStrategy(..))
import Test.Utils (launchCommand, mkAffContainer)

privilegedTest :: Spec Unit
privilegedTest = describe "Privileged Mode" $ do
  it "should set privileged mode correctly" $ do
    dockerDind <- mkAffContainer "docker:dind" $ setPrivilegedMode <<< setWaitStrategy [ LogOutput "API listen on" 1 ]
    res <- withContainer dockerDind $ \c -> do
      launchCommand c [ "docker", "ps" ]
        (\s -> s `shouldInclude` "STATUS")
        (\exitCode -> exitCode `shouldEqual` 0)

    -- Check that the withContainer function succeeded
    case res of
      Left e -> throwError $ error e
      Right _ -> pure unit

