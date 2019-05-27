CREATE TABLE IF NOT EXISTS users (
  id SERIAL PRIMARY KEY,
  google_id TEXT NOT NULL UNIQUE,
  name TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE,
  picture TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (CURRENT_TIMESTAMP AT TIME ZONE 'utc')
);

CREATE TABLE IF NOT EXISTS questions (
  id SERIAL PRIMARY KEY,
  user_id INTEGER NOT NULL,
  text TEXT NOT NULL,
  description TEXT NOT NULL,
  ends_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (CURRENT_TIMESTAMP AT TIME ZONE 'utc'),

  FOREIGN KEY(user_id) REFERENCES users(id)
);

CREATE TABLE IF NOT EXISTS possible_outcomes (
  id SERIAL PRIMARY KEY,
  question_id INTEGER NOT NULL,
  text TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (CURRENT_TIMESTAMP AT TIME ZONE 'utc'),

  FOREIGN KEY(question_id) REFERENCES questions(id)
);

CREATE TABLE IF NOT EXISTS outcomes (
  id SERIAL PRIMARY KEY,
  question_id INTEGER NOT NULL,
  possible_outcome_id INTEGER NOT NULL,
  createdAt TIMESTAMPTZ NOT NULL DEFAULT (CURRENT_TIMESTAMP AT TIME ZONE 'utc'),

  FOREIGN KEY(question_id) REFERENCES questions(id),
  FOREIGN KEY(possible_outcome_id) REFERENCES possible_outcomes(id)
);

CREATE TABLE IF NOT EXISTS forecasts (
  id SERIAL PRIMARY KEY,
  question_id INTEGER NOT NULL,
  user_id INTEGER NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (CURRENT_TIMESTAMP AT TIME ZONE 'utc'),

  FOREIGN KEY(question_id) REFERENCES questions(id),
  FOREIGN KEY(user_id) REFERENCES users(id)
);

CREATE TABLE IF NOT EXISTS forecast_outcomes (
  id SERIAL PRIMARY KEY,
  forecast_id INTEGER NOT NULL,
  possible_outcome_id INTEGER NOT NULL,
  probability INTEGER NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (CURRENT_TIMESTAMP AT TIME ZONE 'utc'),

  FOREIGN KEY(forecast_id) REFERENCES forecasts(id),
  FOREIGN KEY(possible_outcome_id) REFERENCES possible_outcomes(id)
);