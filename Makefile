SRC=src
SPAGO=./node_modules/.bin/spago
DBMIGRATE=./node_modules/.bin/db-migrate
PARCEL=./node_modules/.bin/parcel
UI_MAIN=UI.Main
BUNDLE_LOCATION=static/app.js
BUNDLE_DIR=static
DBCONFIG=db/config.json
MIGDIR=db/migrations

setup:
	# TODO: Don't require installing purescript globally
	# Currently spago looks for `purs` on the global path
	# So we have to for now...
	npm install -g purescript \
	&& npm install \
	&& ${SPAGO} install

bundle: 
	${SPAGO} bundle-app -m ${UI_MAIN} --to ${BUNDLE_LOCATION}

bundle-watch: 
	${SPAGO} bundle-app --watch -m ${UI_MAIN} --to ${BUNDLE_LOCATION}

bundle-minify: bundle
	${PARCEL} build --no-source-maps ${BUNDLE_LOCATION} \
		-o ${BUNDLE_LOCATION} -d ${BUNDLE_DIR}

build: 
	${SPAGO} build

serve: bundle
	${PARCEL} static/index.html

repl:
	${SPAGO} repl