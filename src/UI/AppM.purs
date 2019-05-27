module UI.AppM where

import Prelude

import API.Query as Q
import API.Request as R
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.DateTimeW (DateTimeW(..))
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Data.JSDate as JD
import Repository.Types (PossibleOutcomeNT(..))
import Routing.PushState as PS
import Simple.JSON (write)
import Type.Equality (class TypeEquals, from)
import Capability.Now (class Now)
import Capability.LogMessages (class LogMessages)
import Capability.Navigate (class Navigate, navigateWithState)
import Capability.Resource.Question (class ManageQuestion)
import Capability.Resource.User (class ManageUser)
import Data.Log as Log
import Data.Route (routePath)

type Env =
  { queryURL :: String
  , logLevel :: Log.LogLevel
  , pushStateInterface :: PS.PushStateInterface
  }

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadErrorAppM :: MonadError Error AppM
derive newtype instance monadThrowAppM :: MonadThrow Error AppM

-- | Need this because `Env` is not a newtype
instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance logMessagesAppM :: LogMessages AppM where
  logMsg log = do 
    env <- ask
    liftEffect case env.logLevel of
        Log.Prod -> pure unit
        _        -> Console.log log

instance nowAppM :: Now AppM where
  now = liftEffect $ JD.now <#> DateTimeW


instance navigateAppM :: Navigate AppM where
  navigate r = navigateWithState (write {}) r
  navigateWithState state route = do
    { pushStateInterface } <- ask
    liftEffect $ pushStateInterface.pushState state $ routePath route

instance manageUserAppM :: ManageUser AppM where
  getMe = R.mkRequest Q.GetMe
  getUser = R.mkRequest <<< Q.GetUser
  getUsers = R.mkRequest Q.GetUsers

instance manageQuestionAppM :: ManageQuestion AppM where
  createQuestion q pos = R.mkRequest $ Q.CreateQuestion q (PossibleOutcomeNT <$> pos)
  getQuestion  = R.mkRequest <<< Q.GetQuestion
  getQuestions = R.mkRequest Q.GetQuestions

-- | TODO:
-- instance manageForecastAppM :: ManageForecast AppM where
--   getQuestionForecasts :: DBQuestion -> m DBForecastFull
--   createForecast :: Forecast -> Array ForecastOutcome -> m DBForecastFull
--   getForecast :: Int -> m DBForecastFull