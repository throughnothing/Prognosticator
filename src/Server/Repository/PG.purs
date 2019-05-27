module Repository.PG
  (mkRepository
  ) where

import Prelude

import API.Query (GetUserFilter(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (foldMap)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Database.Postgres as PG
import Database.Postgres.SqlValue (toSql)
import Database.Postgres.Transaction as PGT
import Effect.Aff (Aff, Error, error, throwError)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (Foreign)
import Repository (Repository)
import Repository.Types (DBPossibleOutcome, DBQuestion, DBUser, PossibleOutcome, Question, User, DBQuestionFull)
import Simple.JSON (class ReadForeign)
import Simple.JSON as SJ

type PGConfig = 
  { host :: String
  , database :: String
  , port :: Int
  , user :: String
  , password :: String
  , ssl :: Boolean
  }

-- Make a repository for SqLite
mkRepository :: PGConfig -> Aff (Repository Aff)
mkRepository c = do
  pool <- liftEffect $ PG.mkPool $ conInfo c
  PG.connect pool >>= go
  where
    conInfo = flip PG.connectionInfoFromConfig PG.defaultPoolConfig

    go :: PG.Client -> Aff (Repository Aff)
    go db = pure
        { getUser: getUser db
        , getUsers: getUsers db
        , findOrCreateUser: findOrCreateUser db
        , createQuestion: insertQuestion db
        , getQuestion: getQuestion db
        , getQuestions: getQuestions db
        }

findOrCreateUser :: PG.Client -> User -> Aff (DBUser)
findOrCreateUser db newUser = do
  mExUser <- getUser db (UserByGoogleId newUser.google_id)
  case mExUser of
    Just exUser -> pure exUser
    Nothing -> insertUser db newUser

getUsers :: PG.Client -> Aff (Array DBUser)
getUsers db = PG.query_ readF query db
  where query = q_ "SELECT * FROM users"

getUser :: PG.Client -> GetUserFilter -> Aff (Maybe DBUser)
getUser db filter = do
  let (Tuple query params) = getQuery filter
  PG.queryOne readF query params db
  where
    getQuery (UserByGoogleId id) = Tuple
      (q_ "SELECT created_at::text, * FROM users WHERE google_id=$1") [toSql id]
    getQuery (UserById id) = Tuple
      (q_ "SELECT created_at::text, * FROM users WHERE id=$1") [toSql id]

insertUser :: PG.Client -> User -> Aff DBUser
insertUser db nu = do
  errOnMaybe =<< do PG.queryOne readF q p db
  where
    p = toSql <$> [nu.google_id, nu.name, nu.email, nu.picture ]
    q = q_
      """
      INSERT INTO users (google_id, name, email, picture)
      VALUES ($1,$2,$3,$4)
      RETURNING id, created_at::text, google_id, name, email, picture
      """

insertQuestion :: PG.Client -> Question -> Array PossibleOutcome -> Aff DBQuestionFull
insertQuestion d q nPos = PGT.withTransaction go d
  where
    go db = do
      user <- errOnMaybe =<<
        PG.queryOne
          readF
          (q_ "SELECT created_at::text, * FROM users WHERE id = $1")
          [toSql q.user_id]
          db
      question <- errOnMaybe =<<
        PG.queryOne
          readF
          (q_ 
            """
              INSERT INTO questions (user_id, text, description, ends_at)
              VALUES ($1,$2,$3,$4)
              RETURNING id, created_at::text, user_id, text, description, ends_at::text
            """
          )
        [toSql q.user_id, toSql q.text, toSql q.description, toSql q.ends_at]
        db
      possible_outcomes <- sequence $ insertPossibleOutcome db question <$> nPos
      pure
        { user
        , question
        , possible_outcomes
        }

insertPossibleOutcome :: PG.Client -> DBQuestion -> PossibleOutcome -> Aff DBPossibleOutcome
insertPossibleOutcome db q (nPo) = errOnMaybe =<<
    PG.queryOne
      readF
      (q_ 
        """
        INSERT INTO possible_outcomes (question_id, text)
        VALUES ($1,$2)
        RETURNING id, question_id, text, created_at::text
        """
      )
      [toSql q.id, toSql nPo.text]
      db

getQuestion :: PG.Client -> Int -> Aff DBQuestionFull
getQuestion db id = question >>= \q ->
  {question: _, user: _, possible_outcomes: _} q <$> user q <*> pos q
  where
    question = errOnMaybe =<<
      PG.queryOne readF (q_ "SELECT created_at::text, ends_at::text, * FROM questions WHERE id=$1") [toSql id] db
    user q = errOnMaybe =<<
      PG.queryOne readF (q_ "SELECT created_at::text, * FROM users WHERE id = $1") [toSql q.user_id] db
    pos q = 
      PG.query readF (q_ "SELECT created_at::text, * FROM possible_outcomes WHERE question_id = $1") [toSql q.id] db

getQuestions :: PG.Client -> Aff (Array DBQuestionFull)
getQuestions db = do
  qIds <- questionIds
  sequence $ qIds <#> (\q -> getQuestion db q.id)
  where
    questionIds :: Aff (Array { id :: Int })
    questionIds = PG.query_ readF (q_ "SELECT id FROM questions ORDER BY id DESC") db


errOnMaybe :: ∀ m a. MonadThrow Error m => MonadEffect m => Maybe a -> m a
errOnMaybe = case _ of
    Nothing -> throwError $ error "Failed to get object"
    Just u -> pure u

readF :: ∀ a. ReadForeign a => Foreign -> Either Error a
readF f = lmap (error <<< foldMap show) $ SJ.read f

q_ :: ∀ a. String -> PG.Query a
q_ = PG.Query