module Test.Images where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Effect.Aff (error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Testcontainers (mkContainerFromDockerfile, mkContainerFromDockerfile', setCommand, withContainer)
import Test.Utils (launchCommand)

imagesTest :: Spec Unit
imagesTest =
  describe "Test Images building" $ do
    it "should create a container from a Dockerfile" $ do
      cnt <- mkContainerFromDockerfile "./test/images/" "my-image:latest"

      res <- withContainer (setCommand ["sleep", "360"] cnt) $ \c -> do
        -- figlet has been installed from the Dockerfile
        launchCommand c [ "which", "figlet" ]
          (\s -> s `shouldEqual` "/usr/bin/figlet\n")
          (\code -> code `shouldEqual` 0)

      case res of
        Left e -> throwError $ error e
        Right _ -> pure unit

    it "should create a container from a custom Dockerfile" $ do
      cnt <- mkContainerFromDockerfile' "./test/images" "Dockerfile.alpine" "my-image:latest"
      res <- withContainer (setCommand ["sleep", "360"] cnt) $ \c -> do
        launchCommand c [ "which", "curl" ]
          (\s -> s `shouldEqual` "/usr/bin/curl\n")
          (\code -> code `shouldEqual` 0)

      case res of
        Left e -> throwError $ error e
        Right _ -> pure unit
