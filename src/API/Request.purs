module API.Request where

import Prelude

import API.Query (Query)
import Affjax (printResponseFormatError)
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign (unsafeToForeign)
import Foreign.Class (encode)
import Simple.JSON (class ReadForeign, read)
import Unsafe.Coerce (unsafeCoerce)

-- | TODO: Make a better "Error" type than String
mkRequest 
  :: forall m r a
   . MonadAff m
  => MonadAsk { queryURL :: String | r } m
  => ReadForeign a
  => Query
  -> m (Either String a)
mkRequest query = do
  { queryURL } <- ask
  response <- liftAff $
    AX.post ResponseFormat.json queryURL
    (RequestBody.json $ unsafeCoerce $ encode query)
  case response.body of
    Left e  -> pure $ Left $ printResponseFormatError e
    Right b -> pure $ lmap show (read $ unsafeToForeign b)
