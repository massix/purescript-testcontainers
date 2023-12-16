module Test.Privileged where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), isRight)
import Effect.Aff (error)
import Partial.Unsafe (unsafePartial)
import Test.Assertions (shouldInclude)
import Test.Partials (forceRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.TestContainers (exec, setPrivilegedMode, setWaitStrategy, withContainer)
import Test.TestContainers.Types (WaitStrategy(..))
import Test.Utils (mkAffContainer)

privilegedTest :: Spec Unit
privilegedTest = describe "Privileged Mode" $ do
  it "should set privileged mode correctly" $ do
    dockerDind <- mkAffContainer "docker:dind" $ setPrivilegedMode <<< setWaitStrategy [LogOutput "API listen on" 1]
    res <- withContainer dockerDind $ \c -> do
      execResult <- exec [ "docker", "ps" ] c
      execResult `shouldSatisfy` isRight

      let { output, exitCode } = unsafePartial $ forceRight execResult
      exitCode `shouldEqual` 0
      output `shouldInclude` "STATUS"
      pure unit

    -- Check that the withContainer function succeeded
    case res of
      Left e -> throwError $ error e
      Right _ -> pure unit

