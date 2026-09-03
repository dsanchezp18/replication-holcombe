* ============================================================
* Logit/Probit Exploration: Income Group as a Function of Regulation
* Author: Daniel Sanchez Pazmiño
* Purpose: Stata translation of code/analysis/03_logit_probit_exploration.R
*          (not part of the paper's core results).
* Inputs:  data/final/corruption_final.csv
* Outputs: none (results printed to the log only)
* ============================================================

clear all
set more off

* 0. Setup ----

import delimited "data/final/corruption_final.csv", clear varnames(1)

* 1. Load Data ----
* (loaded above; `hinc` already built by code/cleaning/01_clean_corruption_data.R)

* 2. Analysis ----

logit hinc gdp_pc reg
estimates store log1

probit hinc gdp_pc reg
estimates store prob1

estimates table log1 prob1, star stats(N r2_p)

quietly logit hinc gdp_pc reg
margins, dydx(*)
