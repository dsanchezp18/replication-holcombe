# ============================================================
# Summary Statistics and LaTeX Table Export
# Author: Daniel Sanchez Pazmiño
# Purpose: Produce descriptive-statistics tables and demonstrate
#          LaTeX regression-table export (stargazer), including
#          heteroskedasticity-robust standard errors.
# Inputs:  data/final/corruption_final.rds
# Outputs: outputs/tables/summary_stats.xlsx
#          outputs/tables/cpi_byregion.xlsx
#          outputs/tables/reg1.tex
#          outputs/tables/reg1_labeled.tex
#          outputs/tables/reg1_robust.tex
#          outputs/tables/main_models.tex
# ============================================================

# set.seed(42)

# 0. Setup ----

library(psych)     # descriptive stats, including by-group
library(openxlsx)  # export to xlsx
library(stargazer) # LaTeX regression tables
library(sandwich)  # vcovHC for robust standard errors

corruption_final <- readRDS(file.path("data", "final", "corruption_final.rds"))
tables_dir <- file.path("outputs", "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Load Data ----
# (loaded above)

# 2. Descriptive Tables ----

summary_stats <- describe(corruption_final, na.rm = TRUE)
write.xlsx(summary_stats, file.path(tables_dir, "summary_stats.xlsx"), rowNames = TRUE)

summary_by_region <- describeBy(corruption_final$cci, corruption_final$region, na.rm = TRUE)
summary_by_region_df <- do.call("rbind", summary_by_region)
write.xlsx(summary_by_region_df, file.path(tables_dir, "cpi_byregion.xlsx"), rowNames = TRUE)

stargazer(corruption_final, summary = TRUE, type = "text")

# 3. Analysis ----
# A small, self-contained model set purely to demonstrate table export
# (distinct from the paper's replication models in
# code/analysis/01_main_regressions.R).

reg1 <- lm(cpi ~ lgdp_pc + lpop, data = corruption_final)
reg2 <- lm(cpi ~ lgdp_pc + lpop + reg, data = corruption_final)
reg3 <- lm(cpi ~ lgdp_pc + lpop + efw + govexp, data = corruption_final)
reg4 <- lm(cpi ~ lgdp_pc + lpop + efw + govexp + nat + oil, data = corruption_final)

covariate_labels <- c(
  "Log of GDP per capita", "Log of Population",
  "Economic Freedom of the World Index", "Government expenditure (% of GDP)"
)

# 4. Export ----

stargazer(reg1, out = file.path(tables_dir, "reg1.tex"))

stargazer(
  reg1,
  title = "Corruption explained by political, social and economic variables",
  covariate.labels = covariate_labels,
  dep.var.labels = "Corruption Perceptions Index",
  out = file.path(tables_dir, "reg1_labeled.tex")
)

# Heteroskedasticity-robust standard errors
reg1_robust_se <- sqrt(diag(vcovHC(reg1, type = "HC1")))

stargazer(
  reg1,
  title = "Corruption explained by political, social and economic variables",
  covariate.labels = covariate_labels,
  dep.var.labels = "Corruption Perceptions Index",
  se = list(reg1_robust_se),
  out = file.path(tables_dir, "reg1_robust.tex")
)

stargazer(reg1, reg2, reg3, reg4, out = file.path(tables_dir, "main_models.tex"))

sessionInfo()
