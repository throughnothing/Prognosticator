module Repository.Types where

import Prelude

import Affjax.RequestBody (RequestBody(..))
import Data.DateTimeW (DateTimeW)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)

type FirebaseUser =
  { uid :: String
  , email :: String
  , displayName :: String
  , photoUrl :: String
  }

type WithId r = ( id :: Int | r )
type WithCreatedAt r = ( created_at :: DateTimeW | r )
type DB r = { | (WithId (WithCreatedAt r)) }

type DBUser = { | (WithId UserRows) }
type User = { | UserRows }
type UserRows =  ()
  -- ( google_id :: String
  -- , name :: String
  -- , email :: String
  -- , picture :: String
  -- )

type DBQuestion = DB (QuestionRows)
type Question = { | QuestionRows }
type QuestionRows =
  ( user_id :: Int
  , text :: String
  , description :: String
  , ends_at :: DateTimeW
  )


-- | This is a hack for Generic.Rep on the Query data type
-- | so that we can get de/serialization for free
newtype PossibleOutcomeNT = PossibleOutcomeNT { | PossibleOutcomeRows }
derive instance genericPossibleOutcomeNT :: Generic PossibleOutcomeNT _
instance showPossibleOutcomeNT :: Show PossibleOutcomeNT where show = genericShow
instance decodePossibleOutcomeNT :: Decode PossibleOutcomeNT where decode = genericDecode defaultOptions
instance encodePossibleOutcomeNT :: Encode PossibleOutcomeNT where encode = genericEncode defaultOptions

type DBPossibleOutcome = DB ( question_id :: Int | PossibleOutcomeRows)
type PossibleOutcome = Record (| PossibleOutcomeRows)
type PossibleOutcomeRows = ( text :: String )

type DBOutcome = DB OutcomeRows
type Outcome = { | OutcomeRows }
type OutcomeRows =
  ( question_id :: Int
  , possible_outcome_id :: String
  )

type DBForecast = DB ForecastRows
type Forecast = { | ForecastRows }
type ForecastRows =
  ( question_id :: Int
  , user_id :: Int
  )

-- | This is a hack for Generic.Rep on the Query data type
-- | so that we can get de/serialization for free
newtype ForecastOutcomeNT = ForecastOutcomeNT { | ForecastOutcomeRows }
derive instance genericForecastOutcomeNT :: Generic ForecastOutcomeNT _
instance showForecastOutcomeNT :: Show ForecastOutcomeNT where show = genericShow
instance decodeForecastOutcomeNT :: Decode ForecastOutcomeNT where decode = genericDecode defaultOptions
instance encodeForecastOutcomeNT :: Encode ForecastOutcomeNT where encode = genericEncode defaultOptions

type DBForecastOutcome = DB QuestionRows
type ForecastOutcome = { | ForecastOutcomeRows }
type ForecastOutcomeRows =
  ( forecast_id :: Int
  , possible_outcome_id :: Int
  , probability :: Int
  )

type DBQuestionFull =
  { user :: DBUser
  , question :: DBQuestion
  , possible_outcomes :: Array DBPossibleOutcome
  }

type DBForecastFull = 
  { user :: DBUser
  , forecast :: DBForecast
  , forecast_outcomes :: Array DBForecastOutcome
  }