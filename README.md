# Replicating Holcombe & Boudreaux (2015) [*Regulation & Corruption*](https://link.springer.com/article/10.1007/s11127-015-0263-x)

## Overview
This repository replicates Holcombe & Boudreaux (2015), *Regulation and Corruption*, using data from a later year than the original paper (most results are fairly robust to the change). The main replication is in R; a Stata translation and a small Python side-analysis are also included.

## How to Replicate
1. Clone the repository and open `holcombe-replication.Rproj` in RStudio (or set the project root as your working directory).
2. Run `MASTER.R` from top to bottom. It sources, in order:
   - `code/cleaning/01_clean_corruption_data.R`
   - `code/analysis/01_main_regressions.R`
   - `code/analysis/02_figures.R`
   - `code/analysis/03_logit_probit_exploration.R`
   - `code/analysis/04_summary_tables_export.R`
3. Optionally, run the Stata translation: `code/stata/01_main_regressions.do` (after step 2 has produced `data/final/corruption_final.csv`).
4. Optionally, run the Python side-analysis: `python code/python/01_descriptive_analysis.py` (from the project root, after step 2).

## Project Structure
```
data/raw/           source data, untouched
data/final/         analysis-ready data + saved model objects
code/cleaning/       raw -> final data transformation
code/analysis/       regressions, figures, table export (R)
code/stata/          Stata translation
code/python/         Python side-analysis
outputs/graphs/       figures
outputs/tables/       descriptive & regression tables
documentation/        the source paper
```
Every folder has its own `README.md` with more detail.

## Software & Versions
- R (tidyverse packages: dplyr, ggplot2, readr; plus car, lmtest, sandwich, psych, openxlsx, stargazer, margins)
- Stata 18 (tested via the `stata-mcp` integration)
- Python 3 (pandas, matplotlib, seaborn)

## Data Access
`data/raw/cor1.csv` combines indicators from the World Bank, the Economic Freedom of the World index, and other sources cited in the paper (see `documentation/`). Consult the paper for exact source references.

## Team
Daniel Sanchez Pazmiño (dsanchezp998@gmail.com)
