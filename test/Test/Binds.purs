module Test.Binds (bindTest) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (error, throwError)
import Effect.Class (liftEffect)
import Node.Process as Process
import Test.Assertions (shouldInclude)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Testcontainers (setBindMounts, setCommand, setCopyFilesToContainer, setTmpFs, withContainer)
import Test.Testcontainers.Types (CopyContentToContainer(..), FileMode(..))
import Test.Utils (launchCommand, mkAffContainer)

bindTest :: Spec Unit
bindTest = do
  describe "Binds and Volumes" $ do
    it "should bind single files" $ do
      currentDir <- liftEffect Process.cwd
      alpine <- mkAffContainer "alpine:latest" $
        setCommand [ "sleep", "infinity" ]
          <<< setBindMounts [ { readOnly: true, source: currentDir <> "/test/bound_file.txt", target: "/bound_file.txt" } ]

      res <- withContainer alpine $ \c -> do
        launchCommand c [ "cat", "/bound_file.txt" ]
          (\s -> s `shouldInclude` "hello world from a bounded file\n\n")
          (\exitCode -> exitCode `shouldEqual` 0)

      case res of
        Left err -> throwError $ error err
        Right _ -> pure unit

    it "should bind folders" $ do
      currentDir <- liftEffect Process.cwd
      alpine <- mkAffContainer "alpine:latest" $
        setCommand [ "sleep", "infinity" ]
          <<< setBindMounts [ { readOnly: true, source: currentDir <> "/src/", target: "/sources" } ]

      res <- withContainer alpine $ \c -> do
        launchCommand c [ "ls", "/sources" ]
          (\s -> s `shouldInclude` "Test\n")
          (\exitCode -> exitCode `shouldEqual` 0)

        -- Read only should be respected
        launchCommand c [ "touch", "/sources/a" ]
          (\_ -> pure unit)
          (\exitCode -> exitCode `shouldEqual` 1)

      case res of
        Left err -> throwError $ error err
        Right _ -> pure unit

    it "should copy files and contents to containers" $ do
      currentDir <- liftEffect Process.cwd
      alpine <- mkAffContainer "alpine:latest" $
        setCommand [ "sleep", "infinity" ]
          <<< setCopyFilesToContainer
            [ (FromSource "test/bound_file.txt" "/bound_file.txt" $ FileMode "0644")
            , (FromContent "hello world from copied content" "/copied_content.txt" $ FileMode "0644")
            , (FromDirectory (currentDir <> "/test") "/test" $ FileMode "0644")
            ]

      res <- withContainer alpine $ \c -> do
        launchCommand c [ "cat", "/bound_file.txt" ]
          (\s -> s `shouldEqual` "hello world from a bounded file\n\n")
          (\exitCode -> exitCode `shouldEqual` 0)

        launchCommand c [ "cat", "/copied_content.txt" ] 
          (\s -> s `shouldEqual` "hello world from copied content")
          (\exitCode -> exitCode `shouldEqual` 0)

        launchCommand c [ "ls", "/test" ]
          (\s -> s `shouldInclude` "bound_file.txt")
          (\exitCode -> exitCode `shouldEqual` 0)

      case res of
        Left err -> throwError $ error err
        Right _ -> pure unit

    it "should bind tmpfs volumes" $ do
      alpine <- mkAffContainer "alpine:latest" $
        setCommand [ "sleep", "infinity" ]
          <<< setTmpFs { path: "/tmpfsmount", mountOptions: "rw,noexec,nosuid,size=655536k" }

      res <- withContainer alpine $ \c -> do
        launchCommand c [ "touch", "/tmpfsmount/a" ] 
          (\_ -> pure unit) 
          (\exitCode -> exitCode `shouldEqual` 0)
        launchCommand c [ "mount" ]
          (\s -> s `shouldInclude` "/tmpfsmount")
          (\_ -> pure unit)

      case res of
        Left e -> throwError $ error e
        Right _ -> pure unit

