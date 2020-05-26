module Data.Route where

import Prelude

import Data.Either (hush)
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromMaybe)
import Routing as R
import Routing.Match as M

data Route
  = Home
  | Questions
  | CreateQuestion
  | Question Int
  | Login
  -- | User Int
  | NotFound

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route
instance showRoute :: Show Route where show = genericShow

routePath :: Route -> String
routePath Home = "/"
routePath Login = "/login"
routePath Questions = "/questions"
routePath (Question i) = "/questions/" <> (show i)
routePath CreateQuestion = "/questions/new"
routePath NotFound = "/404"

parseRoute :: String -> Route
parseRoute = fromMaybe NotFound <<< hush <<< R.match match

match :: M.Match Route
match = M.root *> oneOf 
    [ Home <$ M.end
    , Login <$ (M.lit "login" <* M.end)
    , Question <$> (M.lit "questions" *> M.int <* M.end)
    , Questions <$ (M.lit "questions" <* M.end)
    , CreateQuestion <$ (M.lit "questions" <* M.lit "new" <* M.end)
    , NotFound <$ M.nonempty
    ]

