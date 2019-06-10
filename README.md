# Prognosticator

Prognosticator is a web app that allows users to record arbitrary
predictions that they have some opinion / conviction on,
and update and track their accuracy over time.

Prognosticator also serves as a playground for me to
learn [Halogen](https://github.com/slamdata/purescript-halogen)
and to learn about building web apps (server + client) in a
purely functional way with [Purescript](https://github.com/purescript/purescript).


## Design

Prognosticator is written in Purescript, leveraging Node.js, ExpressJS,
Halogen UI framework, PostgreSQL for persistance, and cookie-stored sessions
(via ExpressJS) to avoid needing a session store.

The `main` entry point for the node/express server is located in `src/Server/Main.purs`,
and the `main` entry point for the UI is located in `src/UI/Main.purs`.


## Development

Setting up your dev environment.
You will need to [install nix](https://nixos.org/nix/download.html),
if you don't have it already.

You will also want a PostgreSQL server available, the below
suggests using docker, with the provided docker helper script.

Finally, currently Prognosticator only supports sign-in with Google,
so you will need to have a Google AppID setup in your `.env` for
login and authentication to work.

```
# To run the server, and postgres server,
# you will need the proper environment
# variables set. Copy the .env.template
# file to .env and fill in the  appropriate
# values before sourcing
. .env

# Start Postgres in Docker
# You can stop, rm, and start this container
# later with these commands:
#   - docker stop prognosticator-pg
#   - docker rm prognosticator-pg
#   - docker start prognosticator-pg
db/local_docker.sh

# Load the nix environment for the app
nix-shell

# Install npm deps, spago, and purescript deps
make setup

# Migrate the database up
make db-migrate-up

# Run the server
# This builds the client-side app.js
# and then runs the local server
npm run server

# Now you can view it in your browser
# http://localhost:port
```

## TODO

- [ ] Add sorting / pagination / filtering to queries
- [ ] Add ability to create forecasts on a question
- [ ] Add better error messages for Create Question / Forecast
- [ ] Add statistics for forecasts from the forecast group
- [ ] Add charts / graphs for forecasts over time
- [ ] Implement hot reloading with Parcel (https://github.com/justinwoo/halogen-parcel-hot-reload-demo)
