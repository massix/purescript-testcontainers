module Test.Main (main) where

-------------------------------------------------------------------------------

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Test.Basic (basicTest)
import Test.Binds (bindTest)
import Test.Compose (composeTest)
import Test.Entrypoint (entrypointTest)
import Test.EnvironmentVariables (environmentTest)
import Test.Images (imagesTest)
import Test.Network (networkTest)
import Test.NetworkMode (networkModeTest)
import Test.Ports (portMappingTest)
import Test.Privileged (privilegedTest)
import Test.Spec (describe)
import Test.Spec.Config (Config, defaultConfig)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec')
import Test.WaitStrategy (waitStrategyTest)

--------------------------------------------------------------------------------

main :: Effect Unit
main = launchAff_ $ runSpec' configuration [ consoleReporter ] $ do
  describe "Containers" $ do
    basicTest
    portMappingTest
    bindTest
    privilegedTest
    environmentTest
    waitStrategyTest
    entrypointTest
    networkModeTest
    networkTest
    composeTest
    imagesTest

  where
  -- Set a very high timeout due to docker having to pull all the images..
  configuration :: Config
  configuration = defaultConfig { timeout = Just $ Milliseconds 300_000.0, failFast = true }
