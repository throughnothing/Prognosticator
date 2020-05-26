module Capability.Resource.User where

import Prelude

import Data.Either (Either)
import Halogen (HalogenM, lift)
import Repository.Types (DBUser)

class Monad m <= ManageUser m where
  getMe :: m (Either String DBUser)

instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM s a c o m) where
  getMe = lift getMe