#!/usr/bin/env bash
# Use this script to run a local postgres in docker
# pre-configured to be used with the "dev" db-migrate environment
# https://hub.docker.com/_/postgres/

docker run -p 5432:5432 \
  --name prognosticator-pg \
  -e POSTGRES_USER=$PGUSER \
  -e POSTGRES_PASSWORD=$PGPASSWORD \
  -e POSTGRES_DB=$PGDATABASE \
  postgres  2>&1 >> docker_db.log &