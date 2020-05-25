module Capability.Navigate where

import Prelude

import Halogen (HalogenM, lift)
import Data.Route (Route)

class Monad m <= Navigate m where
  navigate :: Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM s a c o m) where
  navigate = lift <<< navigate