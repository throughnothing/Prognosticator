module Server.App where

import Prelude

import API.Query (GetUserFilter(..), Query(..))
import Capability.Express as CE
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (runExcept, throwError)
import Control.Promise (fromAff)
import Data.Array (length)
import Data.Either (Either(..), hush)
import Data.List (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, error, Error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Express.Foreign as E
import Foreign (Foreign)
import Foreign.Class (decode)
import Node.Express.App (App, post, get, use, useExternal, listenHttp)
import Node.HTTP (Server)
import Repository (Repository)
import Repository.Types (DBUser, ForecastOutcomeNT(..), PossibleOutcomeNT(..))
import Server.App.Env (Env)
import Server.Repository.Validate (validateNewForecast)
import Simple.JSON (read, write)

data ServerRoutes
  = Login
  | GoogleAuth
  | GoogleCb
  | AppJS
  | Root
  | Query
  | Else

routeStr :: ServerRoutes -> String
routeStr Login = "/login"
routeStr GoogleAuth = "/auth/google"
routeStr GoogleCb = "/auth/google/callback"
routeStr AppJS = "/app.js"
routeStr Root = "/"
routeStr Query = "/query"
routeStr Else = "*"

app :: Repository Aff -> Env -> App
app repo c = do
  let mkNewUser = \n -> fromAff $ repo.findOrCreateUser n

  let p = E.mkPassport c mkNewUser

  -- | Setup JSON Body Parser
  useExternal E.jsonBodyParser

  -- | Cookie Session middleware
  useExternal $ E.sessionMiddleware c.cookieSecret

  -- | Initialize Passport + Setup Session
  useExternal $ E.passportInitializeMiddleware p
  useExternal $ E.passportSessionMiddleware p

  -- | Enforce auth on all routes but login ones
  use authEnforcer

  -- | Google Auth Route Handlers
  get (routeStr GoogleAuth) $
      E.passportAuthenticateHandler p
  get (routeStr  GoogleCb) $
      E.passportAuthenticateCallbackHandler
        { failureRedirect: routeStr GoogleAuth
        , successRedirect: routeStr Root
        }
        p

  -- | Route for querying data (API route)
  post (routeStr Query) $ queryHandler repo
  get (routeStr AppJS) $ CE.sendFile "static/app.js"
  get (routeStr Login) $ CE.sendFile "static/login.html"
  -- | All routes serve index.html for pushstate goodness
  get (routeStr Else) $ CE.sendFile "static/index.html"


-- | Handle queries.  This is effectively the API.
queryHandler :: ∀ m
   . MonadAff m
  => MonadThrow Error m
  => CE.ExpressReq m
  => CE.ExpressRes m
  => Repository Aff
  -> m Unit
queryHandler repo = do
      user <- getUser
      query <- getBodyQuery
      liftAff (runQuery repo user query) >>= CE.send

-- | Enforce that we have an authed user for all requests
-- | except the login + login callback routes
authEnforcer :: ∀ m
  .  CE.ExpressH m
  => CE.ExpressReq m
  => CE.ExpressRes m
  => m Unit
authEnforcer = do
  reqPath <- CE.getPath
  mUser <- CE.getField "user"
  if isJust mUser || elem reqPath allowedPaths
    then CE.next
    else CE.redirect $ routeStr Login
  where
    allowedPaths = [routeStr GoogleAuth, routeStr GoogleCb, routeStr Login]

-- | Helper to get the DBUser from the session in a route
getUser :: ∀ m
  .  MonadThrow Error m
  => CE.ExpressReq m
  => m DBUser
getUser = do
  fu <- fromMaybe (write {}) <$> CE.getField "user" 
  let maybeUser = (hush $ read fu) :: Maybe DBUser
  case maybeUser of
    -- | If we're not authenticated,
    -- | don't run any queries
    Nothing -> throwError $ error "Not Authenticated"
    Just user -> pure user

-- | Helper to parse the body in a route
getBodyQuery :: ∀ m
  .  MonadThrow Error m
  => CE.ExpressReq m
  => m Query
getBodyQuery = do
  body <- runExcept <<< decode <$> CE.getBody'
  case body of
    Left e  -> throwError $ error "Error parsing request body"
    Right q -> pure q

runQuery :: ∀ m
  .  Functor m
  => MonadThrow Error m
  => Repository m
  -> DBUser
  -> Query
  -> m Foreign
runQuery r u = case _ of
  GetMe                 -> map write $ r.getUser $ UserByGoogleId u.google_id
  GetUsers              -> write <$> r.getUsers
  (GetUser f)           -> write <$> r.getUser f
  GetQuestions          -> write <$> r.getQuestions
  (GetQuestion id)      -> write <$> r.getQuestion id
  (CreateForecast f xs) -> do
    let unwrapped = xs <#> \(ForecastOutcomeNT x) -> x 
        newForecast = Tuple f unwrapped
    q <- r.getQuestion f.question_id
    -- | TODO: Ensure there is a FO for each PO
    -- | AND that sum(FO.probabality) = 100
    case (validateNewForecast q newForecast) of
      Left e -> throwError $ error $ "Invalid forecast sent: " <> e
      Right (Tuple forecast fos) -> pure (write {})
  (CreateQuestion q xs) ->
    let unwrapped = xs <#> \(PossibleOutcomeNT x) -> x 
    in
      -- | TODO: Only doing this if/the/else b/c NonEmptyArray
      -- | was not immediately easy to encode/decode in t he Query type
      -- | So using an Array, and manually checking length here
      if length unwrapped > 0
      then write <$> (r.createQuestion q unwrapped)
      else throwError $ error  "Need at least 1 possible outcome to create a question!"

-- | Serve the app
serve :: Int -> App -> Effect Server
serve port a = listenHttp a
  port \_ -> log $ "Listening on " <> (show port)
