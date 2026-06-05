# R/00_sandbox_explore_data.R

library(tidyverse)
library(lubridate)

panel_file <- "../data/processed/brazil_credit_monthly_panel_with_mp_shocks.csv"

df <- readr::read_csv(panel_file) |>
  mutate(month = as.Date(month))







# library(readr)
# 
# df <- read_csv(
#   "../data/processed/brazil_credit_monthly_panel_with_mp_shocks.csv",
#   col_types = cols(
#     interest_rate_free_new_operations_total = col_double()
#   )
# )



# Basic overview ----------------------------------------------------------

glimpse(df)

df |>
  summarise(
    start = min(month, na.rm = TRUE),
    end = max(month, na.rm = TRUE),
    n_months = n(),
    n_vars = ncol(df)
  )

# Main variables ---------------------------------------------------------

main_vars <- c(
  "selic_policy_rate",
  "delta_selic",
  "ipca",
  "industrial_output_general",
  "exchange_rate_log_change",
  "free_credit_stock",
  "directed_credit_stock",
  "directed_credit_share",
  "growth_free_credit_stock",
  "growth_directed_credit_stock",
  "interest_rate_free_new_operations_total",
  "interest_rate_directed_new_operations_total"
)

df |>
  select(month, all_of(main_vars)) |>
  summary()



# Missingness ------------------------------------------------------------

df |>
  summarise(across(all_of(main_vars), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") |>
  arrange(desc(n_missing))

# First and last non-missing months ---------------------------------------

df |>
  pivot_longer(-month, names_to = "variable", values_to = "value") |>
  filter(variable %in% main_vars, !is.na(value)) |>
  group_by(variable) |>
  summarise(
    first_month = min(month),
    last_month = max(month),
    n_obs = n(),
    .groups = "drop"
  ) |>
  arrange(first_month)

# Quick plots ------------------------------------------------------------

df |>
  select(month, all_of(main_vars)) |>
  pivot_longer(-month, names_to = "variable", values_to = "value") |>
  ggplot(aes(month, value)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal()

# Focus: free vs directed interest rates ---------------------------------

df |>
  select(
    month,
    interest_rate_free_new_operations_total,
    interest_rate_directed_new_operations_total
  ) |>
  pivot_longer(-month, names_to = "series", values_to = "value") |>
  ggplot(aes(month, value, color = series)) +
  geom_line() +
  theme_minimal()

# Focus: free vs directed credit growth ----------------------------------

df |>
  select(
    month,
    growth_free_credit_stock,
    growth_directed_credit_stock
  ) |>
  pivot_longer(-month, names_to = "series", values_to = "value") |>
  ggplot(aes(month, value, color = series)) +
  geom_line() +
  theme_minimal()








# Things I'd like to plot: 
# - directed/free growth plot
# - directed share
# - inflation gap 
# - output gap 
# - commodity cycle? 

# Note: not only graphs, maybe a table too? min max for the share for example