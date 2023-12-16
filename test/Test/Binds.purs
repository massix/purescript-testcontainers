module Test.Binds (bindTest) where

import Prelude

import Data.Either (Either(..), isRight)
import Effect (Effect)
import Effect.Aff (error, throwError)
import Effect.Class (liftEffect)
import Node.Process as Process
import Partial.Unsafe (unsafePartial)
import Test.Assertions (shouldInclude)
import Test.Partials (forceRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.TestContainers (exec, mkContainer, setBindMounts, setCommand, setCopyFilesToContainer, setTmpFs, withContainer)
import Test.TestContainers.Monad (configure, getContainer, setCommandM, setPrivilegedModeM, setPullPolicyM)
import Test.TestContainers.Types (CopyContentToContainer(..), FileMode(..), PullPolicy(..), TestContainer)
import Test.Utils (launchCommand, mkAffContainer)

bindTest :: Spec Unit
bindTest = do
  describe "Binds and Volumes" $ do
    it "should bind single files" $ do
      alpine <- liftEffect $ do
        currentDir <- Process.cwd
        mkAlpineContainer $
          setBindMounts [{ readOnly: true, source: currentDir <> "/test/bound_file.txt", target: "/bound_file.txt" }]

      res <- withContainer alpine $ \c -> do
        res <- exec [ "cat", "/bound_file.txt" ] c
        res `shouldSatisfy` isRight
        let { exitCode, output } = unsafePartial $ forceRight res
        exitCode `shouldEqual` 0
        output `shouldEqual` "hello world from a bounded file\n\n"

      case res of
        Left err -> throwError $ error err
        Right _ -> pure unit

    it "should bind folders" $ do
      alpine <- liftEffect $ do
        currentDir <- Process.cwd
        mkAlpineContainer $
          setBindMounts [{ readOnly: true, source: currentDir <> "/src/", target: "/sources" }]

      res <- withContainer alpine $ \c -> do
        res <- exec [ "ls", "/sources" ] c
        res `shouldSatisfy` isRight
        let { exitCode, output } = unsafePartial $ forceRight res
        exitCode `shouldEqual` 0
        output `shouldInclude` "Test\n"

        -- Read only should be respected
        res' <- exec [ "touch", "/sources/a" ] c
        res' `shouldSatisfy` isRight
        let { exitCode: exitCode' } = unsafePartial $ forceRight res'
        exitCode' `shouldEqual` 1

      case res of
        Left err -> throwError $ error err
        Right _ -> pure unit

    it "should copy files and contents to containers" $ do
      alpine <- liftEffect $ do
        currentDir <- Process.cwd
        mkAlpineContainer $
          setCopyFilesToContainer 
            [ (FromSource "test/bound_file.txt" "/bound_file.txt" $ FileMode "0644")
            , (FromContent "hello world from copied content" "/copied_content.txt" $ FileMode "0644")
            , (FromDirectory (currentDir <> "/test") "/test" $ FileMode "0644")
            ]
      res <- withContainer alpine $ \c -> do
        res <- exec [ "cat", "/bound_file.txt" ] c
        res `shouldSatisfy` isRight
        let { output } = unsafePartial $ forceRight res
        output `shouldEqual` "hello world from a bounded file\n\n"

        res' <- exec [ "cat", "/copied_content.txt" ] c
        res' `shouldSatisfy` isRight
        let { output: output' } = unsafePartial $ forceRight res'
        output' `shouldEqual` "hello world from copied content"

        res'' <- exec [ "ls", "/test" ] c
        res'' `shouldSatisfy` isRight
        let { output: output'' } = unsafePartial $ forceRight res''

        output'' `shouldInclude` "bound_file.txt"

      case res of
        Left err -> throwError $ error err
        Right _ -> pure unit

    it "should bind tmpfs volumes" $ do
      alpine <- mkAffContainer "alpine:latest" $
        setCommand [ "sleep", "30" ]
        <<< setTmpFs { path: "/tmpfsmount", mountOptions: "rw,noexec,nosuid,size=655536k" }

      res <- withContainer alpine $ \c -> do
        launchCommand c [ "touch", "/tmpfsmount/a" ] (\_ -> pure unit) (\code -> code `shouldEqual` 0)
        launchCommand c [ "mount" ]
          (\s -> s `shouldInclude` "/tmpfsmount")
          (\_ -> pure unit)

      case res of
        Left e -> throwError $ error e
        Right _ -> pure unit

  where
  -- TODO: this can be probably done in a better way
  mkAlpineContainer :: (TestContainer -> TestContainer) -> Effect TestContainer
  mkAlpineContainer action = do 
    let
      cnt = mkContainer "alpine:latest" # configure $ do
        setCommandM [ "sleep", "360" ]
        setPullPolicyM AlwaysPull
        setPrivilegedModeM
        ret <- getContainer
        pure ret

    pure $ action cnt
