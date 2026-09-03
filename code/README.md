# code

## Purpose
All analytical steps, in code, organized by role and by language.

## Contents
- `cleaning/` — raw → final data transformation (R).
- `analysis/` — regressions, figures, and table export that consume `data/final/` (R).
- `stata/` — Stata translation of the main regression sequence.
- `python/` — Python side-analysis (descriptive stats, correlation plot).

## Naming Convention
`NN_verb_description.ext`; the numeric prefix is execution order within its folder.

## Dependencies
`cleaning/` must run before `analysis/`, `stata/`, or `python/`. See the root `MASTER.R` for the R run order.
