module UI.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.PushState as PS
import UI.AppM as AppM
import UI.Component.Router as Router
import Data.Log as Log

main :: Effect Unit
main = HA.runHalogenAff do

  body <- HA.awaitBody
  pushStateInterface <- liftEffect $ PS.makeInterface
  -- Get current URL Path
  currentRoute <- liftEffect $ Router.getCurrentRoute

  let
    queryURL = "/query"
    logLevel = Log.Dev
    env = { queryURL, logLevel, pushStateInterface }

    rootComponent :: H.Component HH.HTML Router.Query Router.Input Void Aff
    rootComponent = H.hoist (AppM.runAppM env) Router.component

  driver <- runUI rootComponent currentRoute body

  -- Listen for pushState changes, and update Router
  void $ liftEffect $
    PS.locations (Router.routeSignal driver) pushStateInterface
