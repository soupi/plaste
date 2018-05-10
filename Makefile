.PHONY: setup

setup:
	stack setup

.PHONY: initdb

initdb:
	psql -U postgres -h 127.0.0.1 postgres -f database_scripts/plaste.ddl; psql -U plaste -h 127.0.0.1 plaste -f database_scripts/database.ddl

.PHONY: build

build:
	stack build

.PHONY: dev

dev:
	stack build --fast --file-watch


.PHONY: run

run:
	stack exec plaste "default" 8080

.PHONY: clean_all

clean_all:
	stack clean; psql -U plaste -f database_scripts/delete_database.ddl; psql -U postgres -f database_scripts/delete_plaste.ddl

.PHONY: clean_db

clean_db:
	psql -U plaste -f database_scripts/delete_database.ddl; psql -U postgres -f database_scripts/delete_plaste.ddl

