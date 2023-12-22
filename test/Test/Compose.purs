module Test.Compose where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..), isLeft, isRight)
import Effect.Aff (Error)
import Effect.Aff.Class (class MonadAff)
import Partial.Unsafe (unsafePartial)
import Test.Assertions (shouldInclude)
import Test.Partials (forceLeft, forceRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Test.TestContainers (exec, getName)
import Test.TestContainers.Compose (composeDown, composeUp, composeUpWithServices, getContainer, mkComposeEnvironment, setEnvironment, setEnvironmentFile, setNoRecreate, setProfiles, setPullPolicy, setRebuild, setWaitStrategy, withCompose, withComposeContainer)
import Test.TestContainers.Types (DockerComposeEnvironment(..), PullPolicy(..), WaitStrategy(..))
import Test.Utils (launchCommand)

composeTest :: Spec Unit
composeTest = do
  describe "Compose" $ do
    describe "Up & Down" $ do
      it "should be able to create a basic environment" $ do
        show defaultEnvironment `shouldEqual` "(CreatedDockerComposeEnvironment ./test/compose)"

      it "should be able to start a compose environment with specified services" $ do
        upped <- composeUpWithServices defaultEnvironment [ "redis" ]
        upped `shouldSatisfy` isRight

        let dce = unsafePartial $ forceRight upped
        checkContainer dce "redis"

        -- This should fail since "alpine-1" is not running
        res <- withComposeContainer dce "alpine-1" \_ -> do
          pure unit

        res `shouldSatisfy` isLeft
        let runError = unsafePartial $ forceLeft res
        runError `shouldEqual` "Cannot get container \"alpine-1\" as it is not running"

        void $ composeDown dce

      it "should be able to start a compose environment with all services" $ do
        upped <- composeUp defaultEnvironment
        upped `shouldSatisfy` isRight

        let dce = unsafePartial $ forceRight upped
        checkContainer dce "redis"
        checkContainer dce "alpine"

        void $ composeDown dce

      it "should be possible to define wait strategies for containers" $ do
        let waitEnv = setWaitStrategy [ LogOutput "Ready to accept connections tcp" 1 ] "redis-1" defaultEnvironment
        upped <- composeUp waitEnv
        upped `shouldSatisfy` isRight
        let env = unsafePartial $ forceRight upped
        checkContainer env "redis"

      it "should be able to set pull policies correctly" $ do
        let pullEnv = setPullPolicy AlwaysPull defaultEnvironment
        let defaultEnv = setPullPolicy DefaultPolicy defaultEnvironment
        upped <- composeUp pullEnv
        upped `shouldSatisfy` isRight
        let env = unsafePartial $ forceRight upped
        checkContainer env "redis"
        void $ composeDown env

        upped' <- composeUp defaultEnv
        upped' `shouldSatisfy` isRight
        let env' = unsafePartial $ forceRight upped
        checkContainer env' "redis"
        void $ composeDown env'

      it "should not recreate the environment" $ do
        upped <- composeUpWithServices defaultEnvironment [ "alpine" ]
        upped `shouldSatisfy` isRight
        let env = unsafePartial $ forceRight upped
        checkContainer env "alpine"
        void $ withComposeContainer env "alpine" \cnt -> do
          void $ exec [ "touch", "/created-file" ] cnt

        upped' <- composeUpWithServices (setNoRecreate defaultEnvironment) [ "alpine" ]
        upped' `shouldSatisfy` isRight
        let env' = unsafePartial $ forceRight upped
        checkContainer env' "alpine"
        void $ withComposeContainer env' "alpine" \cnt -> do
          launchCommand cnt [ "ls", "-l" , "/created-file" ]
            (\s -> s `shouldInclude` "-rw-rw-rw-")
            (\_ -> pure unit)
        void $ composeDown env'

      it "should be able to rebuild images" $ do
        upped <- composeUp $ setRebuild rebuildableEnvironment
        upped `shouldSatisfy` isRight
        let env = unsafePartial $ forceRight upped

        void $ withComposeContainer env "buildedRedis-1" \cnt -> do
          launchCommand cnt [ "cat", "/info.txt" ]
            (\s -> s `shouldEqual` "redis built from dockerfile\n")
            (\code -> code `shouldEqual` 0)

        void $ withComposeContainer env "buildedNginx-1" \cnt -> do
          launchCommand cnt [ "cat", "/info.txt" ]
            (\s -> s `shouldEqual` "nginx built from dockerfile\n")
            (\code -> code `shouldEqual` 0)

        void $ composeDown env

      it "should be able to use profiles correctly" $ do
        upped <- composeUp $ setProfiles [ "cache", "backend" ] profileEnvironment
        upped `shouldSatisfy` isRight
        let env = unsafePartial $ forceRight upped
        checkContainer env "redis"
        checkContainer env "alpine"

        -- This is the only container that should not be running, hence this should fail
        res <- withComposeContainer env "postgres-1" \cnt -> do
          launchCommand cnt [ "env" ]
            (\_ -> pure unit)
            (\_ -> pure unit)

        res `shouldSatisfy` isLeft
        let runError = unsafePartial $ forceLeft res
        runError `shouldEqual` "Cannot get container \"postgres-1\" as it is not running"

        -- Redis should be running as it is in the "cache" profile
        void $ withComposeContainer env "redis-1" \cnt -> do
          launchCommand cnt [ "whoami" ]
            (\s -> s `shouldEqual` "redis\n")
            (\_ -> pure unit)

        -- The same goes for alpine
        void $ withComposeContainer env "alpine-1" \cnt -> do
          launchCommand cnt [ "whoami" ]
            (\s -> s `shouldEqual` "root\n")
            (\_ -> pure unit)

    describe "Environment" $ do

      it "should be able to use environment variables from file" $ do
        upped <- composeUp $ setEnvironmentFile ".env.custom" environmentFileEnvironment
        upped `shouldSatisfy` isRight
        let env = unsafePartial $ forceRight upped
        checkContainer env "alpine"
        void $ withComposeContainer env "alpine-1" $ \cnt -> do
          launchCommand cnt [ "env" ]
            ( \s -> do
                s `shouldInclude` "SOMEVARIABLE=somevalue\n"
                s `shouldInclude` "QUOTEDVARIABLE=some quoted value\n"
            )
            (\_ -> pure unit)

        void $ composeDown env

      it "should be able to use environment variables from array" $ do
        upped <- composeUp $ setEnvironment
          [ { key: "ALPINE_TAG", value: "latest" }
          , { key: "SOMEVARIABLE", value: "value from array" }
          , { key: "QUOTEDVARIABLE", value: "other value from array" }
          ]
          environmentFileEnvironment

        upped `shouldSatisfy` isRight
        let env = unsafePartial $ forceRight upped
        checkContainer env "alpine"
        void $ withComposeContainer env "alpine-1" $ \cnt -> do
          launchCommand cnt [ "env" ]
            ( \s -> do
                s `shouldInclude` "SOMEVARIABLE=value from array\n"
                s `shouldInclude` "QUOTEDVARIABLE=other value from array\n"
            )
            (\_ -> pure unit)

    describe "Utilities" $ do
      it "should be able to 'bracket' the environment" $ do
        res <- withCompose defaultEnvironment $ \dce -> do
          show dce `shouldEqual` "(StartedDockerComposeEnvironment ./test/compose)"
          checkContainer dce "redis"
          checkContainer dce "alpine"

        case res of
          Left e -> fail e
          Right _ -> pure unit
        pure unit

      it "should be able to retrieve a container from the environment" $ do
        res <- withCompose defaultEnvironment $ \dce -> do
          res' <- withComposeContainer dce "redis-1" $ \cnt -> do
            execRes <- exec [ "whoami" ] cnt
            execRes `shouldSatisfy` isRight
            let { output } = unsafePartial $ forceRight execRes
            pure output

          res' `shouldSatisfy` isRight
          pure $ unsafePartial (forceRight res')

        res `shouldSatisfy` isRight
        let output = unsafePartial $ forceRight res
        output `shouldEqual` "root\n"

  where
  checkContainer :: ∀ m. MonadAff m => MonadThrow Error m => DockerComposeEnvironment -> String -> m Unit
  checkContainer dce@(StartedDockerComposeEnvironment _ _ _) cnt = do
    rawCnt <- getContainer dce (cnt <> "-1")
    rawCnt `shouldSatisfy` isRight

    let container = unsafePartial $ forceRight rawCnt
    cntName <- getName container
    cntName `shouldSatisfy` isRight

    let rn = unsafePartial $ forceRight cntName
    rn `shouldEqual` (cnt <> "-1")
  checkContainer _ _ = fail "invalid environment"

  defaultEnvironment = mkComposeEnvironment "./test/compose" [ "docker-compose.yml" ]
  rebuildableEnvironment = mkComposeEnvironment "./test/compose/rebuildable" [ "docker-compose.yml" ]
  environmentFileEnvironment = mkComposeEnvironment "./test/compose/environmentfile" [ "docker-compose.yml" ]
  profileEnvironment = mkComposeEnvironment "./test/compose/profiles" [ "docker-compose.yml" ]
