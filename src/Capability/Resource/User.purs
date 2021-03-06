module Capability.Resource.User where

import Prelude

import API.Query (GetUserFilter)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Repository.Types (DBUser)

class Monad m <= ManageUser m where
  getMe :: m (Either String DBUser)
  getUser :: GetUserFilter -> m (Either String (Maybe DBUser))
  getUsers :: m (Either String (Array DBUser))

instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM s a c o m) where
  getMe = lift getMe
  getUser = lift <<< getUser
  getUsers = lift getUsers