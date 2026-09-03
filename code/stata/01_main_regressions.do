* ============================================================
* Main Regressions: Regulation and Corruption (Holcombe & Boudreaux 2015)
* Author: Daniel Sanchez Pazmiño
* Purpose: Stata translation of code/analysis/01_main_regressions.R —
*          replicate the paper's core OLS regressions of corruption on
*          regulation and controls, with heteroskedasticity-robust SEs.
* Inputs:  data/final/corruption_final.csv
* Outputs: none (results printed to the log only)
* ============================================================

clear all
set more off

* 0. Setup ----

import delimited "data/final/corruption_final.csv", clear varnames(1)

* 1. Load Data ----
* (loaded above; derived variables already built by
*  code/cleaning/01_clean_corruption_data.R)

* 2. Descriptive Stats ----

summarize cpi reg govexp
count if scandinavia == 1
count if pres == 1 // many missing countries

* 3. Analysis ----

* Regression 1: The Scandinavian factor
regress cpi scandinavia, vce(robust)

* Regression 2: Regulation and government size
regress cpi reg govexp, vce(robust)

* Regression 3: Adding the Scandinavian dummy
regress cpi scandinavia reg govexp, vce(robust)

* Regression 4: Presidential vs. parliamentary democracies
regress cpi pres, vce(robust)

* Regression 5: Presidential democracies with controls
regress cpi pres govexp reg, vce(robust)

* Regression 6: Scandinavia with presidential democracy
regress cpi pres scandinavia, vce(robust)

* Regression 7: Scandinavia with the full control set
regress cpi pres scandinavia govexp reg, vce(robust)

* Regressions 8-10: full control set, without foreign aid, dependent
* variable CPI
regress cpi scandinavia agedem prot col_uk lgdp_pc reg govexp lpop nat, vce(robust)

regress cpi scandinavia agedem prot col_uk lgdp_pc reg govexp lpop nat pres, vce(robust)

regress cpi scandinavia agedem prot col_uk lgdp_pc reg govexp lpop nat legint, vce(robust)

correlate reg legint

* Regressions 11-13: same control sets, dependent variable CCI
regress cci scandinavia agedem prot col_uk lgdp_pc reg govexp lpop nat, vce(robust)

regress cci scandinavia agedem prot col_uk lgdp_pc reg govexp lpop nat pres, vce(robust)

regress cci scandinavia agedem prot col_uk lgdp_pc reg govexp lpop nat legint, vce(robust)

* Regressions 14-15: adding natural-resource controls
regress cci scandinavia agedem prot col_uk lgdp_pc reg govexp lpop nat legint oil min, vce(robust)

regress cpi scandinavia agedem prot col_uk lgdp_pc reg govexp lpop nat legint, vce(robust)

* Heteroskedasticity checks (Breusch-Pagan). estat hettest requires a
* classical (non-robust) VCE, so these re-fit without vce(robust); the
* robust-SE results above are unaffected.
regress cci scandinavia agedem prot col_uk lgdp_pc reg govexp lpop nat legint oil min
estat hettest

regress cpi scandinavia agedem prot col_uk lgdp_pc reg govexp lpop nat legint
estat hettest
