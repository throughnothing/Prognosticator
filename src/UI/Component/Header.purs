module UI.Component.Header where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Repository.Types (DBUser)
import Capability.Navigate (class Navigate, navigate)
import UI.Component.HTML.Util (class_, voidHref)
import Data.Route as R

type State = { route :: R.Route, me :: DBUser }

type Slot p = ∀ q. H.Slot q Void p

data Action
  = Navigate R.Route
  | Update State

type Input = { route :: R.Route, me :: DBUser }

component :: forall q o m. Navigate m =>  H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = \i -> Just <<< Update $ i
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
          -- , _navItem (R.Questions) true (questionsActive route) "Questions"
          -- , _navItem (R.CreateQuestion) false false "Forecasts"
          ]
        , HH.img
          [ HP.src $ me.picture
          , class_ "rounded-circle shadow-lg border border-light"
          , HP.height 40 , HP.width 40
          ]
        ]
      ]
    ]
    where
      questionsActive (R.Questions) = true
      questionsActive (R.Question _) = true
      questionsActive _ = false

  _navItem :: ∀ i.  R.Route -> Boolean -> Boolean -> String -> HH.HTML i Action
  _navItem route enabled active name = HH.li
    [ class_ "nav-item"]
    [ HH.a
      [ class_ $ "nav-link"
          <> (if active then " active" else "")
          <> (if not enabled then " disabled" else "")
      , HE.onClick $ \_ -> Just $ Navigate route
      , voidHref
      ]
      [ HH.text name ]
    ]

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction (Navigate r) = navigate r
  handleAction (Update r)   = void $ H.modify $ const r

  _go :: ∀ a. R.Route -> (a -> Maybe Action)
  _go r = const $ Just <<< Navigate $ r