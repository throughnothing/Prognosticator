module UI.Component.Header where

import Prelude

import Capability.Navigate (class Navigate, navigate)
import Data.Maybe (Maybe(..))
import Data.Route as R
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Repository.Types (DBUser)
import UI.Component.HTML.Util (class_, voidHref)

type State = { route :: R.Route, me :: Maybe DBUser }

type Slot p = ∀ q. H.Slot q Void p

data Action
  = Navigate R.Route

type Input = { route :: R.Route, me :: Maybe DBUser }

component :: forall q o m. Navigate m =>  H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
    }
  where

  render :: State -> H.ComponentHTML Action () m
  render { route, me } = HH.div_
    [ HH.nav
      [ class_ "navbar navbar-expand-sm navbar-dark bg-dark" ]
      [ HH.div
        [ class_ "container" ]
        [ HH.a
          [ class_ "navbar-brand"
          , voidHref
          , HE.onClick $ _go R.Home 
          , voidHref
          ]
          [ HH.text "Prognosticator" ]
        , HH.ul
          [ class_ "navbar-nav mr-auto" ]
          [ _navItem (R.CreateQuestion) true (route == R.CreateQuestion) "Ask"
          , _navItem (R.Questions) true (questionsActive route) "Questions"
          , _navItem (R.CreateQuestion) false false "Forecasts"
          ]
        ,
          case me of
            Just user -> HH.img
              [ HP.src $ "/images/person.svg" <> (show user.id)
              , class_ "rounded-circle shadow-lg border border-light"
              , HP.height 40 , HP.width 40
              ]
            Nothing -> HH.text ""
        ]
      ]
    ]
    where
      questionsActive (R.Questions) = true
      questionsActive (R.Question _) = true
      questionsActive _ = false

  _navItem :: R.Route -> Boolean -> Boolean -> String -> HH.HTML _ Action
  _navItem route enabled active name = HH.li
    [ class_ "nav-item"]
    [ HH.a
      [ class_ $ "nav-link"
          <> (if active then " active" else "")
          <> (if not enabled then " disabled" else "")
      , HE.onClick $ _go route
      , voidHref
      ]
      [ HH.text name ]
    ]

  handleAction :: ∀ s a c. Action -> H.HalogenM s a c o m Unit
  handleAction (Navigate r) = navigate r

  _go :: ∀ a. R.Route -> (a -> Maybe Action)
  _go r = const $ Just <<< Navigate $ r