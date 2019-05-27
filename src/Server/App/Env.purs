module Server.App.Env where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Process (lookupEnv)

type Env =
  { google ::
    { clientId :: String
    , clientSecret :: String
    , callbackURL :: String
    }
  , db ::
    { host :: String
    , database :: String
    , port :: Int
    , user :: String
    , password :: String
    , ssl :: Boolean
    }
  , port :: Int
  , cookieSecret :: String
  }

getEnv :: Effect (Maybe Env)
getEnv = do 
  port <- lookupEnv "PORT"
  cookieSecret <- lookupEnv "COOKIE_SECRET"
  clientId <- lookupEnv "GOOGLE_CLIENT_ID"
  clientSecret <- lookupEnv "GOOGLE_CLIENT_SECRET"
  callbackURL <- lookupEnv "GOOGLE_CLIENT_CALLBACK"
  dbHost <- lookupEnv "PGHOST"
  dbUser <- lookupEnv "PGUSER"
  dbPass <- lookupEnv "PGPASSWORD"
  dbName <- lookupEnv "PGDATABASE"
  dbPort <- lookupEnv "PGPORT"
  pure $ { port: _, cookieSecret: _, google: _, db: _ }
    <$> (port >>= fromString)
    <*> cookieSecret
    <*> ({ clientId: _, clientSecret: _, callbackURL: _ }
          <$> clientId
          <*> clientSecret
          <*> callbackURL
        )
    <*> ({ host: _, database: _, port: _, user: _, password: _, ssl: _ }
          <$> dbHost
          <*> dbName
          <*> (dbPort >>= fromString)
          <*> dbUser
          <*> dbPass
          <*> Just false
        )