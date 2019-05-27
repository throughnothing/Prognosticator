module Control.Promise where

import Prelude

import Data.Either (either)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)

fromAff :: ∀ a. Aff a -> Effect (Promise a)
fromAff aff = promise (\succ err -> runAff_ (either err succ) aff)

foreign import data Promise :: Type -> Type

foreign import promise :: ∀ a b.
  ((a -> Effect Unit) -> (b -> Effect Unit) -> Effect Unit) -> Effect (Promise a)