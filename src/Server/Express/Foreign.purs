module Express.Foreign where

import Prelude

import Server.App.Env (Env)
import Control.Promise (Promise)
import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (Response, Request)
import Repository.Types (User, DBUser)

passportAuthenticateHandler :: Passport -> Handler
passportAuthenticateHandler p = HandlerM \req res nxt ->
  liftEffect $ runFn3 (_passportAuthenticateHandler p) req res nxt


passportAuthenticateCallbackHandler
  :: { failureRedirect :: String , successRedirect :: String }
  -> Passport
  -> Handler
passportAuthenticateCallbackHandler o p = HandlerM \req res nxt ->
  liftEffect $ runFn3 (_passportAuthenticateCallbackHandler o p) req res nxt


getField :: String -> HandlerM (Maybe Foreign)
getField field = HandlerM \req _ _ -> do
  liftEffect $ runFn4 _getField Nothing Just req field

foreign import data Passport :: Type

foreign import _getField
  :: Fn4 (Maybe Foreign) (Foreign -> Maybe Foreign) Request String (Effect (Maybe Foreign))

foreign import jsonBodyParser :: Fn3 Request Response (Effect Unit) (Effect Unit)

foreign import sessionMiddleware
  :: String
  -> Fn3 Request Response (Effect Unit) (Effect Unit)

foreign import mkPassport :: Env -> (User -> Effect (Promise DBUser)) -> Passport

foreign import passportSessionMiddleware
  :: Passport
  -> Fn3 Request Response (Effect Unit) (Effect Unit)

foreign import passportInitializeMiddleware
  :: Passport
  -> Fn3 Request Response (Effect Unit) (Effect Unit)

foreign import _passportAuthenticateHandler
  :: Passport
  -> Fn3 Request Response (Effect Unit) (Effect Unit)

foreign import _passportAuthenticateCallbackHandler
  :: { failureRedirect :: String , successRedirect :: String }
  -> Passport
  -> Fn3 Request Response (Effect Unit) (Effect Unit)
