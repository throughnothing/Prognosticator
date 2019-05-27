module Capability.Navigate where

import Prelude

import Foreign (Foreign)
import Halogen (HalogenM, lift)
import Data.Route (Route)

class Monad m <= Navigate m where
  navigate :: Route -> m Unit
  navigateWithState :: Foreign -> Route -> m Unit
  -- logout :: m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM s a c o m) where
  navigate = lift <<< navigate
  navigateWithState r = lift <<< navigateWithState r