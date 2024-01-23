module Test.Images where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..))
import Effect.Aff (error)
import Partial.Unsafe (unsafePartial)
import Test.Partials (forceRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Testcontainers (mkContainerFromDockerfile, mkContainerFromDockerfile', mkContainerFromDockerfileOpts, setCommand, withContainer)
import Test.Utils (launchCommand)

imagesTest :: Spec Unit
imagesTest =
  describe "Test Images building" $ do
    it "should create a container from a Dockerfile" $ do
      cnt <- mkContainerFromDockerfile "./test/images/" "my-image:latest"

      res <- withContainer (setCommand [ "sleep", "360" ] cnt) $ \c -> do
        -- figlet has been installed from the Dockerfile
        launchCommand c [ "which", "figlet" ]
          (\s -> s `shouldEqual` "/usr/bin/figlet\n")
          (\code -> code `shouldEqual` 0)

      case res of
        Left e -> throwError $ error e
        Right _ -> pure unit

    it "should create a container from a custom Dockerfile" $ do
      cnt <- mkContainerFromDockerfile' "./test/images" "Dockerfile.alpine" "my-image:latest"
      res <- withContainer (setCommand [ "sleep", "360" ] cnt) $ \c -> do
        launchCommand c [ "which", "curl" ]
          (\s -> s `shouldEqual` "/usr/bin/curl\n")
          (\code -> code `shouldEqual` 0)

      case res of
        Left e -> throwError $ error e
        Right _ -> pure unit

    it "should be able to fine tune the creation of a container" $ do
      cnt <- mkContainerFromDockerfileOpts "./test/images"
        (Just "DockerfileWithBuildArgs")
        Nothing
        "my-image:latest"
        ( Just
            [ { key: "FIRST_PKG", value: "figlet" }
            , { key: "SECOND_PKG", value: "curl" }
            ]
        )
        (Just false)
      cnt `shouldSatisfy` isRight
      let cnt' = unsafePartial $ forceRight cnt

      res <- withContainer (setCommand [ "sleep", "360" ] cnt') $ \c -> do
        launchCommand c [ "which", "figlet" ]
          (\s -> s `shouldEqual` "/usr/bin/figlet\n")
          (\code -> code `shouldEqual` 0)

        launchCommand c [ "which", "curl" ]
          (\s -> s `shouldEqual` "/usr/bin/curl\n")
          (\code -> code `shouldEqual` 0)

      case res of
        Left e -> throwError $ error e
        Right _ -> pure unit
