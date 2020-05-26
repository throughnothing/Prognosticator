module UI.Component.Router where

import Prelude

import Capability.LogMessages (class LogMessages)
import Capability.Navigate (class Navigate, navigate)
import Capability.Now (class Now)
import Capability.Resource.Question (class ManageQuestion)
import Capability.Session (class ManageSession, getCurrentUser)
import Data.Maybe (Maybe(..))
import Data.Route as R
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Halogen as H
import Halogen.HTML as HH
import Repository.Types (DBUser)
import Routing.PushState as PS
import UI.Component.HTML.Util (class_)
import UI.Component.Header as Header
import UI.Page.CreateQuestion as CreateQuestion
import UI.Page.Home as Home
import UI.Page.Login as Login
import UI.Page.Page404 as P404
import UI.Page.Question as Question
import UI.Page.Questions as Questions
import Web.HTML as WH
import Web.HTML.Location as WL
import Web.HTML.Window as WW

type State = { route :: R.Route, me :: Maybe DBUser }

data Query a
  = NavigateRoute R.Route
  | PathChanged String

data Action
  = EvalQuery (Query Unit)
  | GetMe

type Input = R.Route

type ChildSlots =
  ( header :: Header.Slot Unit
  , login :: Login.Slot Unit
  , home :: Home.Slot Unit
  , createQuestion :: CreateQuestion.Slot Unit
  , question :: Question.Slot Unit
  , questions :: Questions.Slot Unit
  , page404 :: P404.Slot Unit
  )

_header = SProxy :: SProxy "header"
_login = SProxy :: SProxy "login"
_home = SProxy :: SProxy "home"
_createQuestion = SProxy :: SProxy "createQuestion"
_question = SProxy :: SProxy "question"
_questions = SProxy :: SProxy "questions"
_page404 = SProxy :: SProxy "page404"

component :: forall o m
  . Navigate m
  => Monad m
  => LogMessages m
  => Now m
  => ManageSession m
  => ManageQuestion m
  => H.Component HH.HTML Query Input o m
component =
  H.mkComponent
    { initialState: \initialState -> { route: initialState, me: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just GetMe
      }
    }
  where

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route, me } =
    case me of
      Nothing -> HH.slot _login unit Login.component unit absurd
      Just user -> HH.div_
        [ HH.slot _header unit Header.component { route, me: user } absurd
        , HH.div
          [ class_ "container pt-3" ]
          [ case route of
              -- R.Home -> HH.slot _home unit Home.component unit absurd
              R.Home -> HH.slot _question unit Questions.component { questions: Nothing } absurd

              R.CreateQuestion ->
                HH.slot _createQuestion unit CreateQuestion.component user absurd

              (R.Question id) ->
                HH.slot _question unit Question.component id absurd

              R.Questions ->
                HH.slot _questions unit Questions.component { questions: Nothing } absurd

              _ -> HH.slot _page404 unit P404.component unit absurd
          ]
        ]

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction (EvalQuery q) = (handleQuery q) <#> const unit
  handleAction GetMe = getCurrentUser >>= case _ of
    Nothing -> pure unit
    me      -> H.modify_ _ { me = me }

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery (NavigateRoute dest) = _handleQuery true dest
  handleQuery (PathChanged dest) = _handleQuery false $ R.parseRoute dest

  _handleQuery :: ∀ a. Boolean -> R.Route -> H.HalogenM State Action ChildSlots o m (Maybe a)
  _handleQuery doNavigate dest = do
    { route } <- H.get
    when (route /= dest) do
      when doNavigate $ navigate dest
      H.modify_ _ { route = dest }
    pure Nothing

  _go :: ∀ a. R.Route -> (a -> Maybe Action)
  _go r = const $ Just <<< EvalQuery <<< NavigateRoute $ r

routeSignal :: H.HalogenIO Query Void Aff
  -> Maybe PS.LocationState
  -> PS.LocationState
  -> Effect Unit
routeSignal driver _ new = go new.pathname
  where go = void <<< launchAff <<< driver.query <<< PathChanged

getCurrentRoute :: Effect R.Route
getCurrentRoute =
  WH.window
  >>= WW.location
  >>= WL.pathname
  <#> R.parseRoute