# ============================================================
# Figures: Regulation and Corruption (Holcombe & Boudreaux 2015)
# Author: Daniel Sanchez Pazmiño
# Purpose: Exploratory figures relating corruption to economic freedom,
#          income, and region.
# Inputs:  data/final/corruption_final.rds
# Outputs: outputs/graphs/fig_efw_cpi.png
#          outputs/graphs/fig_lgdp_cpi.png
#          outputs/graphs/fig_cpi_by_region.png
#          outputs/graphs/fig_cpi_by_income_group.png
#          outputs/graphs/fig_cpi_histogram.png
# ============================================================

# set.seed(42)

# 0. Setup ----

library(dplyr)
library(ggplot2)

corruption_final <- readRDS(file.path("data", "final", "corruption_final.rds"))
graphs_dir <- file.path("outputs", "graphs")
dir.create(graphs_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Load Data ----
# (loaded above)

# 2. Figures ----

# Scatter: CPI vs. Economic Freedom of the World index
reg_efwcpi <- lm(cpi ~ efw, data = corruption_final)
summary(reg_efwcpi)

fig_efw_cpi <- ggplot(corruption_final, aes(x = efw, y = cpi)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Economic Freedom of the World Index") +
  ylab("Corruption Perceptions Index")

# Scatter: CPI vs. log GDP per capita
fig_lgdp_cpi <- ggplot(corruption_final, aes(x = lgdp_pc, y = cpi)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Log of GDP per capita (2017 PPP Dollars)") +
  ylab("Corruption Perceptions Index")

# Bar plot: mean CPI by region
fig_cpi_by_region <- ggplot(corruption_final, aes(x = region, y = cpi)) +
  geom_bar(stat = "summary", fun = "mean") +
  ylab("Mean Corruption Perceptions Index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot: mean CPI by income group (Taiwan excluded: not a World Bank
# income-group member)
fig_cpi_by_income_group <- corruption_final |>
  filter(country != "Taiwan") |>
  ggplot(aes(x = inc_group, y = cpi)) +
  geom_bar(stat = "summary", fun = "mean") +
  ylab("Mean Corruption Perceptions Index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Histogram of CPI
fig_cpi_histogram <- ggplot(corruption_final, aes(x = cpi)) +
  geom_histogram(binwidth = 2)

# 3. Export ----

ggsave(file.path(graphs_dir, "fig_efw_cpi.png"), fig_efw_cpi, width = 7, height = 5)
ggsave(file.path(graphs_dir, "fig_lgdp_cpi.png"), fig_lgdp_cpi, width = 7, height = 5)
ggsave(file.path(graphs_dir, "fig_cpi_by_region.png"), fig_cpi_by_region, width = 8, height = 5)
ggsave(file.path(graphs_dir, "fig_cpi_by_income_group.png"), fig_cpi_by_income_group, width = 8, height = 5)
ggsave(file.path(graphs_dir, "fig_cpi_histogram.png"), fig_cpi_histogram, width = 7, height = 5)

sessionInfo()
