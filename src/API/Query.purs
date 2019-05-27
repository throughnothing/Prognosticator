module API.Query where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericEncode, genericDecode)
import Repository.Types (Forecast, PossibleOutcomeNT, Question, ForecastOutcomeNT)

-- | data type which representss all possibly queries
data Query
  = GetMe
  | GetUser GetUserFilter
  | GetUsers
  | CreateQuestion Question (Array PossibleOutcomeNT)
  | GetQuestion Int
  | GetQuestions
  | CreateForecast Forecast (Array ForecastOutcomeNT)  

derive instance genericQuery :: Generic Query _
instance showQuery :: Show Query where show = genericShow
instance decodeQuery :: Decode Query where decode = genericDecode defaultOptions
instance encodeQuery :: Encode Query where encode = genericEncode defaultOptions

data GetUserFilter
  = UserById Int
  | UserByGoogleId String

derive instance genericGetUserFilter :: Generic GetUserFilter _
instance showGetUserFilter :: Show GetUserFilter where show = genericShow
instance decodeGetUserFilter :: Decode GetUserFilter where decode = genericDecode defaultOptions
instance encodeGetUserFilter :: Encode GetUserFilter where encode = genericEncode defaultOptions


type QueryRunner f =
  { runQuery :: ∀ a. Decode a => Query -> f a
  }