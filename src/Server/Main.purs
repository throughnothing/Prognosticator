module Server.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Repository.PG as PG
import Server.App (serve, app)
import Server.App.Env (getEnv)

main :: Effect Unit
main = launchAff_ do
  mc <- liftEffect $ getEnv
  case mc of
    Nothing -> logError
    Just conf -> do
      repo <- PG.mkRepository conf.db
      _ <- liftEffect $ run repo conf
      pure unit

  where
    run r c = serve c.port (app r c)
    logError = log "Loading config from environment failed! Terminating."