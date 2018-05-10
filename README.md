Plaste
======

A place to paste your texts.


> License: Apache License 2.0

## Deps

This package requires:

- `stack`
- `postgresql-9.5+`

## Setup

Here are the different `make` options:

- `setup`: Install GHC and deps for the package using stack
- `initdb`: Create the `plaste` role, database and the relevant tables
- `build`: Build the haskell package
- `dev`: Continuously build on file changes - good for development
- `run`: Execute the server with the default settings
- `clean_all`: Delete the package build and the database
- `clean_db`: Drop the tables, the database and the 'plaste' role

## Tokens

To use this service you need a token.

Token are saved in the `tokens.txt` file

## Urls

- `GET host/<pid>` - will show the text of this plaste id as html
- `GET host/raw/<pid>` - will return the text of this plaste id raw
- `POST host/<token>` - will let you add a new plaste

## Limits:

- Up to `1000 * 80` characters per plaste. This is hard coded.
- Up to `10000` plastes at each given time. On each new plaste that is added, the plastes with pid - 10000 of it will be deleted. This is hard coded.
