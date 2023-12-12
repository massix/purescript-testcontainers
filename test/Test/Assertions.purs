module Test.Assertions where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.String.Utils (includes)
import Effect.Aff (Error)
import Test.Spec.Assertions (shouldSatisfy)

shouldInclude :: ∀ m. MonadThrow Error m => String -> String -> m Unit
shouldInclude val comp = val `shouldSatisfy` includes comp
