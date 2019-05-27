module Capability.LogMessages where

import Prelude

import Halogen (HalogenM, lift)

class Monad m <= LogMessages m where
  logMsg :: String -> m Unit


instance logMessagesHalogenM :: LogMessages m => LogMessages (HalogenM s a c o m) where
  logMsg = lift <<< logMsg