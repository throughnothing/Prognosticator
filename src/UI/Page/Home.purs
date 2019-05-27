module UI.Page.Home where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Capability.Navigate (class Navigate, navigate)
import UI.Component.HTML.Util (class_)
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
  render _ = HH.div [ class_ "jumbotron" ]
    [ HH.h1 [ class_ "display-4" ]
      [ HH.text "Welcome to Prognosticator!" ]
    , HH.p [ class_ "lead" ] 
      [ HH.text "Prognosticator lets you test, hone, and track your forecasting skills." ]
    , HH.hr [ class_ "my-4" ]
    , HH.p_
      [ HH.text
        """Have a look at the questions others or asking,
        or ask your own. Will you see the future first?"""
      ]
    , HH.button
      [ class_ "btn btn-lg btn-primary"
      , HE.onClick \_ -> Just $ Navigate R.Questions
      ]
      [ HH.text "Browse Questions" ]
    , HH.button
      [ class_ "btn btn-lg btn-primary ml-4"
      , HE.onClick \_ -> Just $ Navigate R.CreateQuestion
      ]
      [ HH.text "Ask a Question" ]
    ]

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction (Navigate r) = navigate r
