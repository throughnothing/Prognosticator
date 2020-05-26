module Firebase where

import Prelude

import Effect (Effect)

foreign import auth :: Effect Unit
foreign import initializeApp :: Effect Unit