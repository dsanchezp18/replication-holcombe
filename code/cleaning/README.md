# code/cleaning

## Purpose
Transforms `data/raw/` into `data/final/`. The only place derived variables (logs, dummies) are constructed.

## Contents
- `01_clean_corruption_data.R`

## Naming Convention
`NN_clean_description.R`.

## Dependencies
Requires `data/raw/cor1.csv`. Run first, before anything in `code/analysis/`, `code/stata/`, or `code/python/`.
