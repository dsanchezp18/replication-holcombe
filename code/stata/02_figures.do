* ============================================================
* Figures: Regulation and Corruption (Holcombe & Boudreaux 2015)
* Author: Daniel Sanchez Pazmiño
* Purpose: Stata translation of code/analysis/02_figures.R.
* Inputs:  data/final/corruption_final.csv
* Outputs: outputs/graphs/fig_efw_cpi_stata.png
*          outputs/graphs/fig_lgdp_cpi_stata.png
*          outputs/graphs/fig_cpi_by_region_stata.png
*          outputs/graphs/fig_cpi_by_income_group_stata.png
*          outputs/graphs/fig_cpi_histogram_stata.png
* ============================================================

clear all
set more off

* 0. Setup ----

import delimited "data/final/corruption_final.csv", clear varnames(1)
local graphs_dir "outputs/graphs"
capture mkdir "`graphs_dir'"

* 1. Load Data ----
* (loaded above)

* 2. Figures ----

* Scatter: CPI vs. Economic Freedom of the World index
regress cpi efw

twoway (scatter cpi efw) (lfit cpi efw), ///
    xtitle("Economic Freedom of the World Index") ///
    ytitle("Corruption Perceptions Index") legend(off)
graph export "`graphs_dir'/fig_efw_cpi_stata.png", replace width(1600)

* Scatter: CPI vs. log GDP per capita
twoway (scatter cpi lgdp_pc) (lfit cpi lgdp_pc), ///
    xtitle("Log of GDP per capita (2017 PPP Dollars)") ///
    ytitle("Corruption Perceptions Index") legend(off)
graph export "`graphs_dir'/fig_lgdp_cpi_stata.png", replace width(1600)

* Bar plot: mean CPI by region
graph bar (mean) cpi, over(region, label(angle(45))) ///
    ytitle("Mean Corruption Perceptions Index")
graph export "`graphs_dir'/fig_cpi_by_region_stata.png", replace width(1600)

* Bar plot: mean CPI by income group (Taiwan excluded: not a World Bank
* income-group member)
preserve
drop if country == "Taiwan"
graph bar (mean) cpi, over(inc_group, label(angle(45))) ///
    ytitle("Mean Corruption Perceptions Index")
graph export "`graphs_dir'/fig_cpi_by_income_group_stata.png", replace width(1600)
restore

* Histogram of CPI
histogram cpi, width(2) frequency
graph export "`graphs_dir'/fig_cpi_histogram_stata.png", replace width(1600)
