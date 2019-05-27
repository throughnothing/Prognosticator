module Capability.Now where

import Prelude

import Data.DateTimeW (DateTimeW)
import Halogen (HalogenM, lift)

class Monad m <= Now m where
  now :: m DateTimeW

instance nowHalogenM :: Now m => Now (HalogenM s a c o m) where
  now = lift now