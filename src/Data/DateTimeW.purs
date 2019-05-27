module Data.DateTimeW where

import Prelude

import Control.Monad.Except (except)
import Control.Plus (empty)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (floor)
import Data.JSDate as JD
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.NonEmpty ((:|))
import Data.String (joinWith)
import Database.Postgres.SqlValue (class IsSqlValue, toSql)
import Foreign (Foreign, ForeignError(..), unsafeToForeign)
import Foreign.Class (class Decode, class Encode)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)


-- | Making our own type + class so we can have this date class
-- | be ReadForeign and WriteForeign

newtype DateTimeW = DateTimeW JD.JSDate

derive instance genericDateTimeW :: Generic DateTimeW _
instance showDateTimeW :: Show DateTimeW where show = genericShow
derive instance ordDateTimeW :: Ord DateTimeW
derive instance eqDateTimeW :: Eq DateTimeW

instance dateTimeWIsSqlValue :: IsSqlValue DateTimeW where
  toSql (DateTimeW dt) = toSql $ toISOString dt

instance dateTimeWEncode :: Encode DateTimeW where
  encode = writeImpl

instance dateTimeWDecode :: Decode DateTimeW where
  decode = readImpl

instance dateTimeWReadForeign :: ReadForeign DateTimeW where
  readImpl f = DateTimeW <$> mkExcept f
    where
    mkExcept = except <<< (note err) <<< parse
    err = wrap $ ForeignError "Could not parse DateTime" :| empty

instance dateTimeWWriteForeign :: WriteForeign DateTimeW where
  writeImpl (DateTimeW d) = unsafeToForeign d 


parse :: Foreign -> Maybe JD.JSDate
parse = _parse Nothing Just

parseStr :: String -> Maybe JD.JSDate
parseStr = _parse Nothing Just <<< unsafeToForeign

forDisplay :: DateTimeW -> String
forDisplay (DateTimeW d) = joinWith "/" [month, day, year]
  where
  day = show $ floor $ JD.getUTCDate d
  month = show $ floor $ 1.0 + JD.getUTCMonth d
  year = show $ floor $ JD.getUTCFullYear d

foreign import _parse :: ∀ a. Maybe a -> (a -> Maybe a) -> Foreign -> Maybe JD.JSDate

foreign import toISOString :: JD.JSDate -> String