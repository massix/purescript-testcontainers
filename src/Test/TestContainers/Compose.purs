module Test.TestContainers.Compose
  ( mkComposeEnvironment
  , composeUp
  , composeUpWithServices
  , composeDown
  , getContainer
  , withCompose
  , withComposeContainer
  , setWaitStrategy
  , setPullPolicy
  , setRebuild
  , setEnvironment
  , setProfiles
  , setEnvironmentFile
  , setNoRecreate
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Compat (EffectFn3)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn4, runEffectFn3, runEffectFn4)
import Test.TestContainers.Types (CreatedDockerComposeEnvironment, DockerComposeEnvironment(..), FilePath, Image(..), KV, PullPolicy, StartedDockerComposeEnvironment, StartedTestContainer, StoppedDockerComposeEnvironment, TestContainer(..), WaitStrategy)

foreign import mkComposeEnvironmentImpl :: Fn2 FilePath (Array FilePath) CreatedDockerComposeEnvironment
foreign import setWaitStrategyImpl
  :: Fn3
       CreatedDockerComposeEnvironment
       String
       (Array WaitStrategy)
       CreatedDockerComposeEnvironment

foreign import setRebuildImpl
  :: Fn1
       CreatedDockerComposeEnvironment
       CreatedDockerComposeEnvironment

foreign import setNoRecreateImpl :: Fn1 CreatedDockerComposeEnvironment CreatedDockerComposeEnvironment

foreign import setPullPolicyImpl
  :: Fn2
       CreatedDockerComposeEnvironment
       PullPolicy
       CreatedDockerComposeEnvironment

foreign import setEnvironmentFileImpl
  :: Fn2
       CreatedDockerComposeEnvironment
       FilePath
       CreatedDockerComposeEnvironment

foreign import setProfilesImpl
  :: Fn2
       CreatedDockerComposeEnvironment
       (Array String)
       CreatedDockerComposeEnvironment

foreign import setEnvironmentImpl
  :: Fn2
       CreatedDockerComposeEnvironment
       (Array KV)
       CreatedDockerComposeEnvironment

foreign import environmentUpImpl
  :: EffectFn4
       CreatedDockerComposeEnvironment
       (Array String)
       (String -> Either String StartedDockerComposeEnvironment)
       (StartedDockerComposeEnvironment -> Either String StartedDockerComposeEnvironment)
       (Promise (Either String StartedDockerComposeEnvironment))

foreign import getContainerImpl
  :: EffectFn4
       StartedDockerComposeEnvironment
       String
       (String -> Either String StartedTestContainer)
       (StartedTestContainer -> Either String StartedTestContainer)
       (Either String StartedTestContainer)

foreign import environmentDownImpl
  :: EffectFn3
       StartedDockerComposeEnvironment
       (String -> Either String StoppedDockerComposeEnvironment)
       (StoppedDockerComposeEnvironment -> Either String StoppedDockerComposeEnvironment)
       (Promise (Either String StoppedDockerComposeEnvironment))

mkComposeEnvironment :: FilePath -> (Array FilePath) -> DockerComposeEnvironment
mkComposeEnvironment fp fc =
  let
    a = runFn2 mkComposeEnvironmentImpl fp fc
  in
    CreatedDockerComposeEnvironment fp fc a

setWaitStrategy :: Array WaitStrategy -> String -> DockerComposeEnvironment -> DockerComposeEnvironment
setWaitStrategy waitStrategies cnt (CreatedDockerComposeEnvironment fp fc dce) =
  let
    result = runFn3 setWaitStrategyImpl dce cnt waitStrategies
  in
    CreatedDockerComposeEnvironment fp fc result
setWaitStrategy _ _ c = c

setNoRecreate :: DockerComposeEnvironment -> DockerComposeEnvironment
setNoRecreate (CreatedDockerComposeEnvironment fp fc dce) =
  let
    result = runFn1 setNoRecreateImpl dce
  in
    CreatedDockerComposeEnvironment fp fc result
setNoRecreate c = c

setProfiles :: Array String -> DockerComposeEnvironment -> DockerComposeEnvironment
setProfiles profiles (CreatedDockerComposeEnvironment fp fc dce) =
  let
    result = runFn2 setProfilesImpl dce profiles
  in
    CreatedDockerComposeEnvironment fp fc result
setProfiles _ c = c

setEnvironment :: Array KV -> DockerComposeEnvironment -> DockerComposeEnvironment
setEnvironment kvs (CreatedDockerComposeEnvironment fp fc dce) =
  let
    result = runFn2 setEnvironmentImpl dce kvs
  in
    CreatedDockerComposeEnvironment fp fc result
setEnvironment _ c = c

setEnvironmentFile :: FilePath -> DockerComposeEnvironment -> DockerComposeEnvironment
setEnvironmentFile fp (CreatedDockerComposeEnvironment fp' fc dce) =
  let
    result = runFn2 setEnvironmentFileImpl dce fp
  in
    CreatedDockerComposeEnvironment fp' fc result
setEnvironmentFile _ c = c

setRebuild :: DockerComposeEnvironment -> DockerComposeEnvironment
setRebuild (CreatedDockerComposeEnvironment fp fc dce) =
  let
    result = runFn1 setRebuildImpl dce
  in
    CreatedDockerComposeEnvironment fp fc result
setRebuild c = c

setPullPolicy :: PullPolicy -> DockerComposeEnvironment -> DockerComposeEnvironment
setPullPolicy pullPolicy (CreatedDockerComposeEnvironment fp fc dce) =
  let
    result = runFn2 setPullPolicyImpl dce pullPolicy
  in
    CreatedDockerComposeEnvironment fp fc result
setPullPolicy _ c@(StartedDockerComposeEnvironment _ _ _) = c
setPullPolicy _ c@(StoppedDockerComposeEnvironment _ _ _) = c

composeUp :: ∀ m. MonadAff m => DockerComposeEnvironment -> m (Either String DockerComposeEnvironment)
composeUp (CreatedDockerComposeEnvironment fp fc dce) = do
  promise <- liftAff $ toAffE $ runEffectFn4 environmentUpImpl dce [] Left Right
  case promise of
    Left e -> pure $ Left e
    Right cp -> pure $ Right (StartedDockerComposeEnvironment fp fc cp)
composeUp _ = pure $ Left "Can not up this Environment"

composeDown :: ∀ m. MonadAff m => DockerComposeEnvironment -> m (Either String DockerComposeEnvironment)
composeDown (StartedDockerComposeEnvironment fp fc dce) = do
  promise <- liftAff $ toAffE $ runEffectFn3 environmentDownImpl dce Left Right
  case promise of
    Left e -> pure $ Left e
    Right cp -> pure $ Right (StoppedDockerComposeEnvironment fp fc cp)

composeDown _ = pure $ Left "Can not down this Environment"

composeUpWithServices :: ∀ m. MonadAff m => DockerComposeEnvironment -> Array String -> m (Either String DockerComposeEnvironment)
composeUpWithServices (CreatedDockerComposeEnvironment fp fc dce) as = do
  promise <- liftAff $ toAffE $ runEffectFn4 environmentUpImpl dce as Left Right
  case promise of
    Left e -> pure $ Left e
    Right cp -> pure $ Right (StartedDockerComposeEnvironment fp fc cp)
composeUpWithServices _ _ = pure $ Left "Can not up this Environment"

getContainer :: ∀ m. MonadEffect m => DockerComposeEnvironment -> String -> m (Either String TestContainer)
getContainer (StartedDockerComposeEnvironment _ _ dce) cnt = do
  res <- liftEffect $ runEffectFn4 getContainerImpl dce cnt Left Right
  case res of
    Left e -> pure $ Left e
    Right sc -> pure $ Right $ (StartedTestContainer (Image "") sc)
getContainer _ _ = pure $ Left "Could not get container for this Environment"

withCompose :: ∀ m a. MonadAff m => DockerComposeEnvironment -> (DockerComposeEnvironment -> m a) -> m (Either String a)
withCompose dce@(CreatedDockerComposeEnvironment _ _ _) action = do
  upped <- composeUp dce
  case upped of
    Left e -> pure $ Left e
    Right env -> do
      res <- action env
      void $ composeDown env
      pure $ Right res

withCompose _ _ = pure $ Left "Can only run this with a non-started environment"

withComposeContainer :: ∀ m a. MonadAff m => DockerComposeEnvironment -> String -> (TestContainer -> m a) -> m (Either String a)
withComposeContainer dce@(StartedDockerComposeEnvironment _ _ _) container action = do
  cnt <- getContainer dce container
  case cnt of
    Left e -> pure $ Left e
    Right c -> do
      res <- action c
      pure $ Right res

withComposeContainer _ _ _ = pure $ Left "Can only run this on a started environment"
