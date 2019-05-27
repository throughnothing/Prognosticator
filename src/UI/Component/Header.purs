module UI.Component.Header where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Repository.Types (DBUser)
import Capability.Navigate (class Navigate, navigate)
import UI.Component.HTML.Util (class_, voidHref)
import Data.Route as R

type State = { route :: R.Route, me :: Maybe DBUser }

type Slot p = ∀ q. H.Slot q Void p

data Action
  = Navigate R.Route
  | Update State

type Input = { route :: R.Route, me :: Maybe DBUser }

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
          [ class_ "navbar-brand pointer"
          , voidHref
          , HE.onClick $ _go R.Home 
          ]
          [ HH.text "Prognosticator" ]
        , HH.ul
          [ class_ "navbar-nav mr-auto" ]
          [ _navItem (Navigate R.Home) true (route == R.Home) "Home"
          , _navItem (Navigate R.Questions) true (route == R.Questions) "Questions"
          , _navItem (Navigate R.CreateQuestion) false false "Forecasts"
          ]
        , HH.img
          [ HP.src $ fromMaybe "/images/person.svg" $ _.picture <$> me
          , class_ "rounded-circle shadow-lg border border-light"
          , HP.height 40 , HP.width 40
          ]
        ]
      ]
    ]

  _navItem :: ∀ i.  Action -> Boolean -> Boolean -> String -> HH.HTML i Action
  _navItem action enabled active name = HH.li
    [ class_ "nav-item"]
    [ HH.a
      [ class_ $ "nav-link pointer"
          <> (if active then " active" else "")
          <> (if not enabled then " disabled" else "")
      , HE.onClick $ \_ -> Just action
      ]
      [ HH.text name ]
    ]

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction (Navigate r) = navigate r
  handleAction (Update r)   = void $ H.modify $ const r

  _go :: ∀ a. R.Route -> (a -> Maybe Action)
  _go r = const $ Just <<< Navigate $ r