module UI.Page.Login where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import UI.Component.HTML.Util (class_)

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
    [ class_ "container col-4" ]
    [ HH.div [ class_ "card mx-auto p-4 jumbotron mt-5" ]
      [ HH.h3 [ class_ "card-title d-inline-block mx-auto mb-3" ]
        [ HH.text "Prognosticator" ]
      , HH.a
        [ HP.href "/auth/google", class_ "d-block mx-auto btn btn-primary mt-4" ]
        [ HH.text "Log In" ]
      ] 
    ]