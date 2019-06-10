module Capability.Session where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Repository.Types (DBUser)

class Monad m <= ManageSession m where
  getCurrentUser :: m (Maybe DBUser)

instance manageSessionHalogenM :: ManageSession m => ManageSession (HalogenM s a c o m) where
  getCurrentUser = lift getCurrentUser