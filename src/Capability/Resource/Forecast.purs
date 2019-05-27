module Capability.Resource.Forecast where

import Prelude

import Repository.Types (DBForecastFull, Forecast, ForecastOutcome, DBQuestion)

class Monad m <= ManageForecast m where
  getQuestionForecasts :: DBQuestion -> m DBForecastFull
  createForecast :: Forecast -> Array ForecastOutcome -> m DBForecastFull
  getForecast :: Int -> m DBForecastFull