module UI.Page.Question where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Repository.Types (DBQuestionFull)
import Capability.LogMessages (class LogMessages, logMsg)
import Capability.Resource.Question (class ManageQuestion, getQuestion)
import UI.Component.HTML.Util (class_)
import Unsafe.Coerce (unsafeCoerce)

type State =
  { questionId :: Int
  , question :: Maybe DBQuestionFull
  }

type Slot p = ∀ q. H.Slot q Void p

type Input = Int

data Action
  = LoadQuestion

component :: forall q o m
   . ManageQuestion m
  => LogMessages m
  => H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just LoadQuestion
      }
    }
  where

  initialState :: Int -> State
  initialState id = { questionId: id, question: Nothing }

  render :: State -> H.ComponentHTML Action () m
  render st = HH.div
    [ class_ "container"]
    [ case st.question of
        Nothing ->  _loading
        Just q -> HH.div
          []
          [ HH.h3_ [ HH.text q.question.text ]
          , HH.hr_ 
          , HH.div_ [ HH.text q.question.description ]
          , HH.hr_
          , HH.div_ $
            q.possible_outcomes <#> (\po -> HH.div_ [HH.text po.text] )
          ]
    ]

  _loading :: ∀ i. HH.HTML i Action
  _loading = HH.div_ [ HH.text "Loading..." ]

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction LoadQuestion = do
    {questionId} <- H.get
    getQuestion questionId >>= case _ of
      Left err -> pure unit
      Right q -> do
        logMsg $ unsafeCoerce q
        H.modify_ _ { question = Just q }
