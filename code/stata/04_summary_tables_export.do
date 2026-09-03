* ============================================================
* Summary Statistics and Table Export
* Author: Daniel Sanchez Pazmiño
* Purpose: Stata translation of code/analysis/04_summary_tables_export.R.
*          Uses only Stata's built-in commands (no estout/outreg2), so
*          regression tables are shown via `estimates table` rather than
*          exported to a formatted .tex file — see the note below.
* Inputs:  data/final/corruption_final.csv
* Outputs: outputs/tables/summary_stats_stata.xlsx
*          outputs/tables/cpi_byregion_stata.xlsx
* ============================================================

clear all
set more off

* 0. Setup ----

import delimited "data/final/corruption_final.csv", clear varnames(1)
local tables_dir "outputs/tables"
capture mkdir "`tables_dir'"

* 1. Load Data ----
* (loaded above)

* 2. Descriptive Tables ----

putexcel set "`tables_dir'/summary_stats_stata.xlsx", replace
putexcel A1 = "variable"
putexcel B1 = "mean"
putexcel C1 = "sd"
putexcel D1 = "min"
putexcel E1 = "max"
putexcel F1 = "n"

local row = 2
foreach v of varlist cpi cci reg govexp efw gdp_pc pop lgdp_pc lpop legint agedem {
    quietly summarize `v'
    putexcel A`row' = "`v'"
    putexcel B`row' = (r(mean))
    putexcel C`row' = (r(sd))
    putexcel D`row' = (r(min))
    putexcel E`row' = (r(max))
    putexcel F`row' = (r(N))
    local row = `row' + 1
}

* Descriptive stats by region for cci
levelsof region, local(regions)
putexcel set "`tables_dir'/cpi_byregion_stata.xlsx", replace
putexcel A1 = "region"
putexcel B1 = "mean_cci"
putexcel C1 = "sd_cci"
putexcel D1 = "n"

local row = 2
foreach region_name of local regions {
    quietly summarize cci if region == "`region_name'"
    putexcel A`row' = "`region_name'"
    putexcel B`row' = (r(mean))
    putexcel C`row' = (r(sd))
    putexcel D`row' = (r(N))
    local row = `row' + 1
}

* 3. Analysis ----
* A small, self-contained model set purely to demonstrate table export
* (distinct from the paper's replication models in 01_main_regressions.do).

regress cpi lgdp_pc lpop
estimates store reg1

regress cpi lgdp_pc lpop reg
estimates store reg2

regress cpi lgdp_pc lpop efw govexp
estimates store reg3

regress cpi lgdp_pc lpop efw govexp nat oil
estimates store reg4

* 4. Export ----

* Built-in combined view of the four models (console/log only). Producing
* a formatted .tex file the way R's stargazer(..., out = ...) does needs
* a user-written package (estout/outreg2), which is not installed here.
estimates table reg1 reg2 reg3 reg4, b(%9.3f) stats(N r2) star
