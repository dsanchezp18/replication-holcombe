# ============================================================
# Descriptive Analysis: Corruption Cross-Country Dataset
# Author: Daniel Sanchez Pazmiño
# Purpose: Small Python side-analysis (descriptive stats and a
#          correlation pairplot); the main replication is done in R.
# Inputs:  data/final/corruption_final.csv
# Outputs: outputs/tables/descriptive_stats_python.xlsx
# ============================================================

import os

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# 0. Setup ----

# Paths are relative to the project root, not the script's own folder,
# so this only works when run from the project root (see MASTER.R /
# run instructions in the top-level README).
data_path = os.path.join("data", "final", "corruption_final.csv")
tables_dir = os.path.join("outputs", "tables")
os.makedirs(tables_dir, exist_ok=True)

# 1. Load Data ----

df = pd.read_csv(data_path)
print(df.head())

# 2. Analysis ----

summary_df = df.describe()
print(summary_df)

correlation_vars = df[["cci", "lgdp_pc", "agedem", "efw", "lpop"]].dropna()

# 3. Export ----

summary_df.to_excel(os.path.join(tables_dir, "descriptive_stats_python.xlsx"))

sns.pairplot(correlation_vars)
plt.savefig(os.path.join("outputs", "graphs", "fig_correlation_pairplot.png"))
plt.show()
