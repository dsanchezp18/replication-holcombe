# data

## Purpose
Holds the project's data at every stage, separated so raw data is never modified in place.

## Contents
- `raw/` — data as received, untouched.
- `final/` — analysis-ready data and saved model objects produced by `code/cleaning/`.

## Naming Convention
`snake_case`, no spaces. Analysis-ready files are suffixed `_final`.

## Dependencies
`final/` is produced by `code/cleaning/01_clean_corruption_data.R`; run it (or `MASTER.R`) before any analysis script.
