module Repository where

import Data.Maybe (Maybe)
import API.Query (GetUserFilter)
import Repository.Types (DBUser, PossibleOutcome, Question, User, DBQuestionFull)

-- | Represents a repository of persisted data
type Repository f = 
  { getUser :: GetUserFilter -> f (Maybe DBUser)
  , getUsers :: f (Array DBUser)
  , findOrCreateUser :: User -> f DBUser
  , createQuestion :: Question -> Array PossibleOutcome -> f DBQuestionFull
  , getQuestion :: Int -> f DBQuestionFull
  , getQuestions :: f (Array DBQuestionFull)
  }