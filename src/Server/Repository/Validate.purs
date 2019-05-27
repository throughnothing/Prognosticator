module Server.Repository.Validate where

import Prelude

import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (find, foldl)
import Data.Maybe (maybe)
import Data.Set (Set, fromFoldable)
import Data.Tuple (Tuple(..))
import Repository.Types (DBPossibleOutcome, DBQuestionFull, Forecast, ForecastOutcome)


validateNewForecast
  :: DBQuestionFull
  -> Tuple Forecast (Array ForecastOutcome)
  -> Either String (Tuple Forecast (Array ForecastOutcome))
validateNewForecast q t@(Tuple f fos) = lengthsMatch >>= outcomeIdsMatch
  where
  outcomeIdsMatch _ =
    if allFosInPoSet
    then Right t
    else Left "No ForecastOutcome for every PossibleOutcome found."
    where
      poSet :: Set DBPossibleOutcome
      poSet = fromFoldable q.possible_outcomes

      allFosInPoSet :: Boolean
      allFosInPoSet = foldl (\b a -> b && match a) true poSet
        where
          match po = maybe false (const true) $
            find (\fo -> po.id == fo.possible_outcome_id) fos

  lengthsMatch =
    if length fos == length q.possible_outcomes
    then Right t
    else Left "Must have same number of ForecastOutcomes as PossibleOutcomes."
