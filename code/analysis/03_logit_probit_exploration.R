# ============================================================
# Logit/Probit Exploration: Income Group as a Function of Regulation
# Author: Daniel Sanchez Pazmiño
# Purpose: Small package-exploration exercise (not part of the paper's
#          core results): predict a high-income dummy from GDP per
#          capita and regulation using logit/probit.
# Inputs:  data/final/corruption_final.rds
# Outputs: none (console/report output only)
# ============================================================

# set.seed(42)

# 0. Setup ----

library(dplyr)
library(margins)
library(stargazer)

corruption_final <- readRDS(file.path("data", "final", "corruption_final.rds"))

# 1. Load Data ----
# (loaded above; `hinc` and `america` dummies already built in the
# cleaning script)

# 2. Analysis ----

log1 <- glm(hinc ~ gdp_pc + reg, data = corruption_final, family = binomial(link = "logit"))
summary(log1)

prob1 <- glm(hinc ~ gdp_pc + reg, data = corruption_final, family = binomial(link = "probit"))
summary(prob1)

stargazer(log1, prob1, type = "text")

margins(log1) |> summary()

sessionInfo()
