module Test.Utils where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (isRight)
import Effect.Aff (Error)
import Effect.Aff.Class (class MonadAff)
import Partial.Unsafe (unsafePartial)
import Test.Partials (forceRight)
import Test.Spec.Assertions (shouldSatisfy)
import Test.TestContainers (exec, mkContainer)
import Test.TestContainers.Monad (WithContainer, configure, getContainer)
import Test.TestContainers.Types (class IsImage, TestContainer)

launchCommand :: ∀ m. MonadAff m => MonadThrow Error m => TestContainer -> Array String -> (String -> m Unit) -> (Int -> m Unit) -> m Unit
launchCommand c cmds vOutput vCode = do
  cmd <- exec cmds c
  cmd `shouldSatisfy` isRight

  let { output, exitCode } = unsafePartial $ forceRight cmd
  vOutput output
  vCode exitCode

mkAffContainerM :: ∀ a m. IsImage a => MonadAff m => a -> WithContainer Unit -> m TestContainer
mkAffContainerM img conf = do
  let
    cnt = mkContainer img # configure $ do
      conf
      res <- getContainer
      pure res
  pure cnt

mkAffContainer :: ∀ a m. IsImage a => MonadAff m => a -> (TestContainer -> TestContainer) -> m TestContainer
mkAffContainer img conf = pure <$> conf $ mkContainer img
