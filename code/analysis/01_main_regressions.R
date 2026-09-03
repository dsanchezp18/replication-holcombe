# ============================================================
# Main Regressions: Regulation and Corruption (Holcombe & Boudreaux 2015)
# Author: Daniel Sanchez Pazmiño
# Purpose: Replicate the paper's core OLS regressions of corruption
#          (CPI, CCI) on regulation and controls, with heteroskedasticity-
#          robust inference, and compare against the paper's findings.
# Inputs:  data/final/corruption_final.rds
# Outputs: data/final/main_regression_models.rds
# ============================================================

# set.seed(42)

# 0. Setup ----

library(dplyr)
library(car)    # heteroskedasticity-robust errors (hccm) and hypothesis testing
library(lmtest) # coeftest(), bptest()

corruption_final <- readRDS(file.path("data", "final", "corruption_final.rds"))

# 1. Load Data ----
# (loaded above; already analysis-ready via code/cleaning/01_clean_corruption_data.R)

# 2. Descriptive Stats ----

mean_cpi <- mean(corruption_final$cpi, na.rm = TRUE)
mean_reg <- mean(corruption_final$reg, na.rm = TRUE)
mean_govexp <- mean(corruption_final$govexp, na.rm = TRUE)

sd_cpi <- sd(corruption_final$cpi, na.rm = TRUE)
sd_reg <- sd(corruption_final$reg, na.rm = TRUE)
sd_govexp <- sd(corruption_final$govexp, na.rm = TRUE)

n_scandinavia <- corruption_final |> filter(scandinavia == 1) |> nrow()
n_presidential <- corruption_final |> filter(pres == 1) |> nrow() # many missing countries

# 3. Analysis ----

# Regression 1: The Scandinavian factor
reg_scand <- lm(cpi ~ scandinavia, data = corruption_final)
summary(reg_scand)
coeftest(reg_scand, vcov = hccm)
# CPI here is inverted relative to the paper's index, and less is explained
# overall, but the result is heteroskedasticity-robust.

# Regression 2: Regulation and government size
reg_govsize <- lm(cpi ~ reg + govexp, data = corruption_final)
summary(reg_govsize)
coeftest(reg_govsize, vcov = hccm)
# Consistent with the paper as long as `reg` is coded so higher = more
# freedom; government expenditure's effect is small and loses significance
# under White errors.

# Regression 3: Adding the Scandinavian dummy
reg_scg <- lm(cpi ~ scandinavia + reg + govexp, data = corruption_final)
summary(reg_scg)
coeftest(reg_scg, vcov = hccm)
# Scandinavia stays less corrupt net of reg and govexp; same pattern holds
# under White errors.

# Regression 4: Presidential vs. parliamentary democracies
reg_parl <- lm(cpi ~ pres, data = corruption_final)
summary(reg_parl)
coeftest(reg_parl, vcov = hccm)
# Presidential democracies tend to be more corrupt; robust to
# heteroskedasticity.

# Regression 5: Presidential democracies with controls
reg_parlet <- lm(cpi ~ pres + govexp + reg, data = corruption_final)
summary(reg_parlet)
coeftest(reg_parlet, vcov = hccm)
# `pres` loses significance once government size is accounted for.

# Regression 6: Scandinavia with presidential democracy
reg_parlscan <- lm(cpi ~ pres + scandinavia, data = corruption_final)
summary(reg_parlscan)
coeftest(reg_parlscan, vcov = hccm)
# Confirms the paper; robust and strengthens significance.

# Regression 7: Scandinavia with the full control set
reg_scan1 <- lm(cpi ~ pres + scandinavia + govexp + reg, data = corruption_final)
summary(reg_scan1)
coeftest(reg_scan1, vcov = hccm)
# Mostly consistent, though `pres` loses significance here, possibly due
# to missing data.

# Practical effect of the first four covariates (regression 7)
beta_pres <- coef(reg_scan1)["pres"]
beta_scandinavia <- coef(reg_scan1)["scandinavia"]
beta_govexp <- coef(reg_scan1)["govexp"]
beta_reg <- coef(reg_scan1)["reg"]
# The practical effect of regulation is non-trivial: about 60% of a
# standard deviation of the CPI index.

# Regression 8-10: Full control set, without foreign aid, dependent
# variable CPI
reg_1 <- lm(cpi ~ scandinavia + agedem + prot + col_uk + lgdp_pc + reg + govexp + lpop + nat,
            data = corruption_final)
summary(reg_1)
coeftest(reg_1, vcov = hccm)
# Broadly consistent; scandinavia loses significance (possibly biased
# toward 0), agedem is very significant, protestant share has a similar
# sign but weaker significance, UK colonial origin is not significant.
# Robust to heteroskedasticity, with a better R^2 than the simpler models.

reg_2 <- lm(cpi ~ scandinavia + agedem + prot + col_uk + lgdp_pc + reg + govexp + lpop + nat + pres,
            data = corruption_final)
summary(reg_2)
coeftest(reg_2, vcov = hccm)
# Adding `pres` changes coefficients substantially, likely because it
# drops the sample size sharply; R^2 rises but watch for this artifact.

reg_3 <- lm(cpi ~ scandinavia + agedem + prot + col_uk + lgdp_pc + reg + govexp + lpop + nat + legint,
            data = corruption_final)
summary(reg_3)
coeftest(reg_3, vcov = hccm)
# Legal integrity predicts corruption better than regulation here,
# consistent with `reg` and `legint` being highly correlated (see below),
# which may have biased `reg` upward in the simpler specifications.

cor_reg_legint <- cor(corruption_final$reg, corruption_final$legint, use = "complete.obs")

# Regressions 11-13: same control sets, dependent variable CCI
reg_1a <- lm(cci ~ scandinavia + agedem + prot + col_uk + lgdp_pc + reg + govexp + lpop + nat,
             data = corruption_final)
summary(reg_1a)
coeftest(reg_1a, vcov = hccm)

reg_2a <- lm(cci ~ scandinavia + agedem + prot + col_uk + lgdp_pc + reg + govexp + lpop + nat + pres,
             data = corruption_final)
summary(reg_2a)
coeftest(reg_2a, vcov = hccm)

reg_3a <- lm(cci ~ scandinavia + agedem + prot + col_uk + lgdp_pc + reg + govexp + lpop + nat + legint,
             data = corruption_final)
summary(reg_3a)
coeftest(reg_3a, vcov = hccm)

# Regressions 14-15: adding natural-resource controls (extension beyond
# the paper's original specification)
reg_3aa <- lm(cci ~ scandinavia + agedem + prot + col_uk + lgdp_pc + reg + govexp + lpop + nat + legint + oil + min,
              data = corruption_final)
summary(reg_3aa)
coeftest(reg_3aa, vcov = hccm)

reg_3ab <- lm(cpi ~ scandinavia + agedem + prot + col_uk + lgdp_pc + reg + govexp + lpop + nat + legint,
              data = corruption_final)
summary(reg_3ab)
coeftest(reg_3ab, vcov = hccm)
# Natural resources appear to increase corruption when measured with CPI.

# Heteroskedasticity checks on the extended models
bptest(reg_3aa)
bptest(reg_3ab)

# 4. Export ----

main_regression_models <- list(
  reg_scand = reg_scand, reg_govsize = reg_govsize, reg_scg = reg_scg,
  reg_parl = reg_parl, reg_parlet = reg_parlet, reg_parlscan = reg_parlscan,
  reg_scan1 = reg_scan1, reg_1 = reg_1, reg_2 = reg_2, reg_3 = reg_3,
  reg_1a = reg_1a, reg_2a = reg_2a, reg_3a = reg_3a,
  reg_3aa = reg_3aa, reg_3ab = reg_3ab
)
saveRDS(main_regression_models, file.path("data", "final", "main_regression_models.rds"))

sessionInfo()
