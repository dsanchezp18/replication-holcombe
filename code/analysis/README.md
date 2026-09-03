# code/analysis

## Purpose
Uses `data/final/` to produce the paper's replicated results, figures, and exported tables.

## Contents
- `01_main_regressions.R` — the paper's core OLS regression sequence (CPI/CCI on regulation and controls).
- `02_figures.R` — exploratory ggplot2 figures.
- `03_logit_probit_exploration.R` — a package-exploration exercise (logit/probit on a high-income dummy), not part of the paper's core results.
- `04_summary_tables_export.R` — descriptive-statistics tables and a LaTeX regression-table export demo (own, simpler model set, separate from `01_main_regressions.R`).

## Naming Convention
`NN_verb_description.R`; numeric prefix is execution order.

## Dependencies
Requires `data/final/corruption_final.rds` (from `code/cleaning/`). `04_summary_tables_export.R` only depends on `data/final/`, not on `01`'s saved models.
