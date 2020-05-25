module UI.Main where

import Prelude

import API.Query (Query(..))
import API.Request (mkRequestURL)
import Data.Either (hush)
import Data.Log as Log
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (new)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.PushState as PS
import Simple.JSON (write)
import UI.AppM as AppM
import UI.Component.Router as Router

main :: Effect Unit
main = HA.runHalogenAff do

  body <- HA.awaitBody

  maybeMe <- hush <$> (mkRequestURL "/query" GetMe)

  { pushStateI
  , currentRoute
  , currentUser
  } <- liftEffect $ do
    pushStateI <- PS.makeInterface
    currentRoute <- Router.getCurrentRoute
    currentUser <- new maybeMe
    pure { pushStateI, currentRoute, currentUser}

  let
    queryURL = "/query"
    logLevel = Log.Dev
    env =
      { queryURL
      , logLevel
      , currentUser
      , pushState : pushStateI.pushState (write {})
      }

    rootComponent :: H.Component HH.HTML Router.Query Router.Input Void Aff
    rootComponent = H.hoist (AppM.runAppM env) Router.component

  driver <- runUI rootComponent currentRoute body

  -- Listen for pushState changes, and update Router
  void $ liftEffect $
    PS.locations (Router.routeSignal driver) pushStateI
