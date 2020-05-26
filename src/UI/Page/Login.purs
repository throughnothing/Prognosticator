module UI.Page.Login where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (id_)

type State = Unit

type Slot p = ∀ q. H.Slot q Void p

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval
    }
  where

  render :: State -> H.ComponentHTML Unit () m
  render _ = HH.div
    [ id_ "firebaseui-auth-container" ]
    [ HH.text "Login" ]
