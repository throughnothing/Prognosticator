module Capability.Resource.Question where

import Prelude

import Data.Either (Either)
import Halogen (HalogenM, lift)
import Repository.Types (PossibleOutcome, DBQuestionFull, Question)

class Monad m <= ManageQuestion m where
  createQuestion :: Question -> Array PossibleOutcome -> m (Either String DBQuestionFull)
  getQuestion :: Int -> m (Either String DBQuestionFull)
  getQuestions :: m (Either String (Array DBQuestionFull))

instance manageQuestionHalogenM :: ManageQuestion m => ManageQuestion (HalogenM s a c o m) where
  createQuestion q = lift <<< createQuestion q
  getQuestion = lift <<< getQuestion
  getQuestions = lift getQuestions