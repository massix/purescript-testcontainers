module Test.Testcontainers.Network
  ( mkNetwork
  , startNetwork
  , getId
  , getName
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Compat (EffectFn1, runEffectFn1)
import Effect.Class (class MonadEffect, liftEffect)
import Test.Testcontainers.Types (GenericNetwork, Network(..), NetworkId, NetworkName, StartedNetwork)

foreign import mkNetworkImpl :: (GenericNetwork -> Network) -> Network
foreign import startNetworkImpl :: EffectFn1 GenericNetwork (Promise StartedNetwork)
foreign import getIdImpl :: EffectFn1 StartedNetwork NetworkId
foreign import getNameImpl :: EffectFn1 StartedNetwork NetworkName

mkNetwork :: Network
mkNetwork = mkNetworkImpl GenericNetwork

startNetwork :: ∀ m. MonadAff m => Network -> m (Either String Network)
startNetwork (GenericNetwork n) = do
  res <- liftAff $ toAffE $ liftEffect $ runEffectFn1 startNetworkImpl n
  pure $ Right $ StartedNetwork res
startNetwork _ = pure $ Left "Cannot start a stopped or an already started network"

getId :: ∀ m. MonadEffect m => Network -> m (Either String NetworkId)
getId (StartedNetwork n) = do
  r <- liftEffect $ runEffectFn1 getIdImpl n
  pure $ Right r
getId _ = pure $ Left "Could not retrieve ID of a Stopped Network"

getName :: ∀ m. MonadEffect m => Network -> m (Either String NetworkName)
getName (StartedNetwork n) = do
  r <- liftEffect $ runEffectFn1 getNameImpl n
  pure $ Right r
getName _ = pure $ Left "Could not retrieve name of a Stopped Network"

