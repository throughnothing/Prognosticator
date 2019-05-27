module UI.Page.CreateForecast where

import Prelude

import Capability.LogMessages (class LogMessages, logMsg)
import Capability.Resource.Question (class ManageQuestion, createQuestion)
import Data.Array (uncons)
import Data.Array.NonEmpty (NonEmptyArray, cons', length, snoc, toArray, unsnoc, updateAt)
import Data.DateTimeW (DateTimeW(..), parseStr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Traversable (sequence)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Repository.Types (DBUser, DBQuestionFull)
import UI.Component.HTML.Util (class_, voidHref)
import Unsafe.Coerce (unsafeCoerce)

type State =
  { me :: DBUser
  , question :: DBQuestionFull
  }

type Slot p = ∀ q. H.Slot q Void p

type Input =
  { me :: DBUser
  , question :: DBQuestionFull
  }

data Action
  = Validate
  | Submit

component :: forall q o m
   . ManageQuestion m
  => LogMessages m
  => H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
    }
  where

  initialState :: Input -> State
  initialState { me, question }= 
    { me
    , question
    }

  render :: State -> H.ComponentHTML Action () m
  render st = HH.div [ class_ "container" ]
    [ HH.h2
      [ class_ "mt-4" ]
      [ HH.text "Create a Forecast" ]
    ]

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction _ = pure unit