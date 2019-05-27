module UI.Page.Page404 where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Capability.Navigate (class Navigate, navigate)
import Data.Route as R

type State = Unit

type Slot p = ∀ q. H.Slot q Void p

data Action = Navigate R.Route

component :: forall q i o m. Navigate m =>  H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where

  render :: State -> H.ComponentHTML Action () m
  render _ = HH.div_
    [ HH.text "Woa, slow down.  You're probably seeing the future before us, because this page doesn't exist yet!"]

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction (Navigate r) = navigate r
