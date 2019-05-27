module UI.Page.Questions where

import Prelude

import Capability.Navigate (class Navigate, navigate)
import Capability.Resource.Question (class ManageQuestion, getQuestions)
import Data.DateTimeW (forDisplay)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Route as R
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Repository.Types (DBQuestionFull)
import UI.Component.HTML.Util (class_)

type State =
  { questions :: Maybe (Array DBQuestionFull)
  }

type Slot p = ∀ q. H.Slot q Void p

data Action
  = LoadQuestions
  | GoToQuestion Int

component :: forall q o m
   . ManageQuestion m
  => Navigate m
  => H.Component HH.HTML q State o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just LoadQuestions
        }
    }
  where

  render :: State -> H.ComponentHTML Action () m
  render st = HH.div
    [ class_ "container"]
    [ case st.questions of
        Nothing ->  _loading
        Just qs -> _questions qs
    ]

  _loading :: ∀ i. HH.HTML i Action
  _loading = HH.div_ [ HH.text "Loading..." ]

  _questions :: ∀ i.  Array DBQuestionFull -> HH.HTML i Action
  _questions qs = HH.div_ $ qs <#> question
    where
      question q = HH.div [ class_ "card mb-3" ]
        [ HH.div [ class_ "card-body" ]
          [ HH.h5 [ class_ "card-title" ] 
            [ HH.a
              [ HP.href "javascript:void(0);"
              , HE.onClick \_ -> Just $ GoToQuestion q.question.id
              ]
              [ HH.text q.question.text ]
            ]
          , HH.p [ class_ "card-text" ]
            [ HH.text q.question.description ]
          , HH.p [ class_ "card-text" ]
            [ HH.text $ q.user.name <> " -- " <> (forDisplay q.question.ends_at)]
          ]
        ]

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction (GoToQuestion id) = navigate $ R.Question id
  handleAction LoadQuestions = getQuestions >>= case _ of
    Left err -> pure unit
    Right qs -> H.modify_ _ { questions = Just qs }
