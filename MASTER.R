# ============================================================
# Master Script: Regulation and Corruption Replication
# Author: Daniel Sanchez Pazmiño
# Purpose: Single entry point that runs the full R pipeline in order,
#          from raw data to figures and exported tables. Run from the
#          project root (or open holcombe-replication.Rproj first).
# Inputs:  data/raw/cor1.csv
# Outputs: data/final/*, outputs/graphs/*, outputs/tables/*
# ============================================================

source(file.path("code", "cleaning", "01_clean_corruption_data.R"))
source(file.path("code", "analysis", "01_main_regressions.R"))
source(file.path("code", "analysis", "02_figures.R"))
source(file.path("code", "analysis", "03_logit_probit_exploration.R"))
source(file.path("code", "analysis", "04_summary_tables_export.R"))

# The Stata translation (code/stata/01_main_regressions.do) and the
# Python side-analysis (code/python/01_descriptive_analysis.py) are
# independent, language-specific replications and are not sourced here.
