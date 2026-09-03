# code/stata

## Purpose
Full Stata translation of the R analysis pipeline (`code/analysis/`), for cross-checking the results in a second package.

## Contents
- `01_main_regressions.do` — translates `01_main_regressions.R`. Robust (HC1) standard errors via `vce(robust)`; no clustering (cross-sectional data, one observation per country). `estat hettest` is run after re-fitting classically, since Stata disallows it after `vce(robust)`.
- `02_figures.do` — translates `02_figures.R`. Writes PNGs to `outputs/graphs/*_stata.png`.
- `03_logit_probit_exploration.do` — translates `03_logit_probit_exploration.R`.
- `04_summary_tables_export.do` — translates `04_summary_tables_export.R`. Uses only Stata's built-in commands (`putexcel`, `estimates table`) — no `estout`/`outreg2`/`asdoc`, which aren't installed, so tables export to `.xlsx` rather than formatted `.tex` (see the note in the file). Ask if you want the `.tex` output too; it needs one of those packages installed first.

## Naming Convention
`NN_verb_description.do`, matching the R script it translates.

## Dependencies
Requires `data/final/corruption_final.csv` (from `code/cleaning/01_clean_corruption_data.R`). All four files tested end-to-end via the `stata-mcp` integration.
