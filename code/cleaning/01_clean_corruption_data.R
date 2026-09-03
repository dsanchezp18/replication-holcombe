# ============================================================
# Clean Corruption Cross-Country Dataset
# Author: Daniel Sanchez Pazmiño
# Purpose: Build the analysis-ready dataset used by every downstream
#          R, Stata, and Python script, so derived variables (logs,
#          dummies) are constructed exactly once.
# Inputs:  data/raw/cor1.csv
# Outputs: data/final/corruption_final.rds
#          data/final/corruption_final.csv
# ============================================================

# set.seed(42)

# 0. Setup ----

library(dplyr)
library(readr)

raw_data_path <- file.path("data", "raw", "cor1.csv")
final_dir <- file.path("data", "final")
dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Load Data ----

corruption_raw <- read_csv(raw_data_path, show_col_types = FALSE)

# 2. Clean / Transform ----

corruption_final <- corruption_raw |>
  mutate(
    col_uk = if_else(col == "GBR", 1, 0),
    lgdp_pc = log(gdp_pc),
    lpop = log(pop),
    lfor = log(foraid),
    # High-income dummy: upper-middle-income countries or the World Bank
    # "High income" region bucket
    hinc = if_else(inc_group == "Upper middle income" | region == "High income", 1, 0),
    america = if_else(region %in% c("Latin America & Caribbean", "North America"), 1, 0)
  ) |>
  mutate(across(c(lgdp_pc, lpop, lfor), \(x) if_else(is.infinite(x), NA_real_, x)))

# 3. Export ----

saveRDS(corruption_final, file.path(final_dir, "corruption_final.rds"))
# na = "" (rather than readr's default "NA" text) so Stata's `import
# delimited` and pandas both read missing values correctly without extra
# options.
write_csv(corruption_final, file.path(final_dir, "corruption_final.csv"), na = "")

message("Wrote ", nrow(corruption_final), " rows to ", final_dir)

sessionInfo()
