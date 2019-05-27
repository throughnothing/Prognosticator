module UI.Page.CreateQuestion where

import Prelude

import Capability.LogMessages (class LogMessages, logMsg)
import Capability.Navigate (class Navigate, navigate)
import Capability.Now (class Now, now)
import Capability.Resource.Question (class ManageQuestion, createQuestion)
import Data.Array (uncons)
import Data.Array.NonEmpty (NonEmptyArray, cons', length, snoc, toArray, unsnoc, updateAt)
import Data.DateTimeW (DateTimeW(..), parseStr)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Route as R
import Data.Traversable (sequence)
import Data.Util (nonEmptyStr)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties as Hp
import Repository.Types (DBUser)
import UI.Component.HTML.Util (class_, voidHref)

type State =
  { me :: DBUser
  , question :: Maybe String
  , description :: Maybe String
  , ends_at :: Maybe DateTimeW 
  , possible_outcomes :: NonEmptyArray (Maybe String)
  , valid :: Maybe ValidState
  , status :: Status
  }

data Status = Filling | Submitting

type ValidState =
  { question :: String
  , description :: String
  , ends_at :: DateTimeW
  , possible_outcomes :: NonEmptyArray String
  }

type Slot p = ∀ q. H.Slot q Void p

type Input = DBUser

data Action
  = AddPO
  | RemPO
  | Update Field
  | Validate
  | Submit

data Field
  = Question String
  | Description String
  | EndsAt String
  | PossibleOutcome Int String

component :: forall q o m
   . ManageQuestion m
  => LogMessages m
  => Navigate m
  => Now m
  => H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
    }
  where

  initialState :: DBUser -> State
  initialState mu = 
    { me: mu
    , question: Nothing
    , description: Nothing
    , possible_outcomes: cons' Nothing []
    , ends_at: Nothing
    , valid: Nothing
    , status: Filling
    }

  render :: State -> H.ComponentHTML Action () m
  render st = HH.div [ class_ "container" ]
    [ HH.h2
      []
      [ HH.text "Ask a Question" ]
    , HH.form [ class_ "mt-2"]
      [ HH.div [ class_ "form-group" ]
        [ HH.label_ [ HH.text "Your Question" ]
        , HH.input
          [ class_ "form-control"
          , HP.placeholder "Make your question clear and specific"
          , HE.onValueInput \s -> Just $ Update $ Question s
          , HP.value $ fromMaybe "" st.question
          , Hp.enabled $
              case st.status of
                Submitting -> false
                _ -> true
          ]
        ]
      , HH.div [ class_ "form-group" ]
        [ HH.label_ [ HH.text "Detail" ]
        , HH.textarea
          [ class_ "form-control"
          , HP.placeholder "Provide more detail / background on your question"
          , HP.rows 3
          , HE.onValueInput \s -> Just $ Update $ Description s
          , HP.value $ fromMaybe "" st.description
          , HP.enabled $
              case st.status of
                Submitting -> false
                _ -> true
          ]
        ]
      , HH.div [ class_ "form-group" ]
        [ HH.label [ class_ "mb-n2" ]
          [ HH.text "Question End Date" ]
        , HH.input
          [ class_ "form-control mt-2"
          , HP.placeholder "When will the answer to this question be known? (MM/DD/YYY)"
          , HE.onValueInput $ \s -> Just $ Update $ EndsAt s
          , HP.enabled $
              case st.status of
                Submitting -> false
                _ -> true
          ]
        ]
      , HH.div [ class_ "form-group" ]
        [ HH.label [ class_ "mb-n2" ]
          [ HH.text "Possible Outcomes" ]
        , HH.a
          [ class_ "btn btn-small btn-outline-dark ml-3"
          , voidHref
          , HE.onClick \_ -> Just AddPO
          ]
          [ HH.text "+" ]
        , HH.a
          [ class_ $ "btn btn-small btn-outline-dark ml-1"
              <> if length st.possible_outcomes <= 1
                 then " disabled"
                 else ""
          , voidHref
          , HE.onClick \_ -> Just RemPO
          ]
          [ HH.text "-" ]

        , HH.div_ $
          toArray $ mapWithIndex
            (\idx po -> HH.input
              [ class_ "form-control mt-2"
              , HP.placeholder "Yes / No"
              , HP.value $ fromMaybe "" po
              , HE.onValueInput $ \s -> Just $ Update $ PossibleOutcome idx s
              , HP.enabled $
                  case st.status of
                    Submitting -> false
                    _ -> true
              ])
            (st.possible_outcomes)
        ]
      , HH.a
        [ class_ $ "btn btn-primary mt-3"
            <> if isJust st.valid
               then ""
               else " disabled"
        , voidHref
        , HE.onClick \_ -> Just Submit
        ]
        [ HH.text "Create Question" ]
      ]
    ]

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction Validate = H.modify_ _validate

  handleAction Submit = do
    H.modify_ _ { status = Submitting }
    st <- H.get
    let vst = _validate st
    case vst.valid of
      Just v -> do
        qFull <- createQuestion
          { user_id: vst.me.id
          , text: v.question
          , description: v.description
          , ends_at: v.ends_at
          }
          (toArray $ { text: _ } <$> v.possible_outcomes)
        case qFull of
          -- | TODO: Handle Failure
          Left err -> do
            H.modify_ _ { status = Filling }
            logMsg "Error submitting form..."
          Right q -> navigate $ R.Question q.question.id
      -- | TODO: Show error messages
      _-> do
        H.modify_ _ { status = Filling }
        logMsg "Form was NOT valid..."

  handleAction AddPO = do
    H.modify_
      \s -> _validate s { possible_outcomes = snoc s.possible_outcomes Nothing}

  handleAction RemPO = do
    H.modify_
      \s -> _validate s { possible_outcomes = newList s.possible_outcomes }
    where
      newList pos =  fromMaybe pos $ headTail pos <#> mkNE
      headTail ps = uncons (unsnoc ps).init
      mkNE {head, tail} = cons' head tail

  handleAction (Update f) = 
    case f of
      Question s -> H.modify_ \st -> _validate st { question = nonEmptyStr s}
      Description s -> H.modify_ \st -> _validate st { description = nonEmptyStr s}
      EndsAt s -> do
        n <- now
        let dtTmp = DateTimeW <$> parseStr s
            -- Make sure the date entered is > now(), if valid
            dt  = dtTmp >>= (\x -> if x > n then Just x else Nothing)
        H.modify_ \st -> _validate st { ends_at = dt }
      PossibleOutcome idx s -> do
        st <- H.get
        let newPos = update idx s st.possible_outcomes
            newState = st { possible_outcomes = newPos }
        H.modify_ \_ -> _validate newState
        where update idx s ps = fromMaybe ps $ updateAt idx (nonEmptyStr s) ps

  _validate :: State -> State
  _validate st = st { valid = valid }
    where
    valid = 
      { question: _, description: _, possible_outcomes: _, ends_at: _ }
      <$> st.question
      <*> st.description
      <*> (sequence st.possible_outcomes)
      <*> st.ends_at