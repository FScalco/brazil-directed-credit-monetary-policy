# R/00_sandbox_explore_data.R


library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(knitr)
library(kableExtra)

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
#  "delta_selic",
  "ipca",
  "ipca_12m",
  "industrial_output_general",
  "exchange_rate_log_change",
#  "free_credit_stock",
#  "directed_credit_stock",
  "directed_credit_share",
#  "growth_free_credit_stock",
#  "growth_directed_credit_stock",
  "interest_rate_free_new_operations_total",
  "interest_rate_directed_new_operations_total",
  "inflation_gap",
  "icbr_commodities",
  "growth_icbr_commodities"
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
  pivot_longer(-c(month, focus_date), names_to = "variable", values_to = "value") |>
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

df |>
  select(month, focus_ipca_12m_expectation, ipca_12m) |>
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





################################################################################
######################Actual Plots##############################################


# ------------- Selic rate

df |>
  ggplot(aes(x = month, y = selic_policy_rate)) +
  geom_line() +
  labs(
    x = NULL,
    y = "Selic interest rate"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 13)
  )


ggsave(
  filename = "figures/descriptive/selic_rate.png",
  plot = last_plot(),
  width = 8,
  height = 4.5,
  units = "in"
)





# - - - - -- - - - - - - - - - - - - - - - - -- - - - - - - - -
# - - - - -- - - - - - - - - - - - - - - - - -- - - - - - - - -

# -------------- Credit growth rates

df |>
  select(
    month,
    growth_free_credit_stock,
    growth_directed_credit_stock
  ) |>
  pivot_longer(-month, names_to = "series", values_to = "value") |>
  mutate(
    series = recode(
      series,
      growth_free_credit_stock = "free credit",
      growth_directed_credit_stock = "directed credit"
    )
  ) |>
  ggplot(aes(month, value, color = series)) +
  geom_line() +
  labs(
    x = NULL,
    y = "Percentage growth rate",
    color = NULL
  ) +
  theme_minimal() + 
  theme(
  axis.text = element_text(size = 9),
  axis.title.y = element_text(size = 9),
  legend.text = element_text(size = 9)
)

ggsave(
  filename = "figures/descriptive/growth_free_vs_directed.png",
  plot = last_plot(),
  width = 8,
  height = 4.5,
  units = "in"
)

# - - - - -- - - - - - - - - - - - - - - - - -- - - - - - - - -
# - - - - -- - - - - - - - - - - - - - - - - -- - - - - - - - -


# ------------- Direct credit share

df |>
  ggplot(aes(x = month, y = directed_credit_share)) +
  geom_line() +
  labs(
    x = NULL,
    y = "Directed credit share"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 13)
  )


ggsave(
  filename = "figures/descriptive/directed_share.png",
  plot = last_plot(),
  width = 8,
  height = 4.5,
  units = "in"
)

# - - - - -- - - - - - - - - - - - - - - - - -- - - - - - - - -
# - - - - -- - - - - - - - - - - - - - - - - -- - - - - - - - -

# ---------------- inflation gap 

df |>
  select(
    month,
    inflation_target,
    focus_ipca_12m_expectation
  ) |>
  pivot_longer(-month, names_to = "series", values_to = "value") |>
  mutate(
    series = recode(
      series,
      inflation_target = "Inflation target",
      focus_ipca_12m_expectation = "Inflation expectation"
    )
  ) |>
  ggplot(aes(month, value, color = series)) +
  geom_line() +
  labs(
    x = NULL,
    y = "Annual inflation rate",
    color = NULL
  ) +
  theme_minimal() + 
  theme(
    axis.text = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    legend.text = element_text(size = 9)
  )

ggsave(
  filename = "figures/descriptive/inflation_gap.png",
  plot = last_plot(),
  width = 8,
  height = 4.5,
  units = "in"
)





# - - - - -- - - - - - - - - - - - - - - - - -- - - - - - - - -
# - - - - -- - - - - - - - - - - - - - - - - -- - - - - - - - -

# ---------------- output gap 

df |>
  select(
    month,
    ibc_br_sa,
    ibc_br_trend
  ) |>
  pivot_longer(-month, names_to = "series", values_to = "value") |>
  mutate(
    series = recode(
      series,
      ibc_br_sa = "IBC-Br",
      ibc_br_trend = "IBC-Br trend"
    )
  ) |>
  ggplot(aes(month, value, color = series)) +
  geom_line() +
  labs(
    x = NULL,
    y = "IBC-Br index",
    color = NULL
  ) +
  theme_minimal() + 
  theme(
    axis.text = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    legend.text = element_text(size = 9)
  )

ggsave(
  filename = "figures/descriptive/output_gap.png",
  plot = last_plot(),
  width = 8,
  height = 4.5,
  units = "in"
)


################################################################################
######################Actual Figures##############################################


# -------------- Interest rate table --------------- #

# Variables for summary table
interest_vars <- c(
  "interest_rate_new_operations_total",
  "interest_rate_free_new_operations_total",
  "interest_rate_directed_new_operations_total"
)


# Create summary table
interest_summary_table <- df %>%
  select(all_of(interest_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(
    Mean   = mean(value, na.rm = TRUE),
    Median = median(value, na.rm = TRUE),
    SD     = sd(value, na.rm = TRUE),
    Min    = min(value, na.rm = TRUE),
    Max    = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    variable = recode(
      variable,
      interest_rate_new_operations_total = "Total credit",
      interest_rate_free_new_operations_total = "Free credit",
      interest_rate_directed_new_operations_total = "Directed credit"
    ),
    across(c(Mean, Median, SD, Min, Max), ~ round(.x, 2))
  )

# Print in R console
interest_summary_table

# Export publication-ready LaTeX table
latex_table <- interest_summary_table %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 2,
    caption = "Descriptive statistics for interest rates on new credit operations",
    col.names = c("Credit category", "Mean", "Median", "SD", "Min", "Max"),
    align = c("l", "r", "r", "r", "r", "r")
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 10,
    position = "center"
  ) %>%
  add_header_above(
    c(" " = 1, "Interest rate on new operations" = 5)
  ) %>%
  footnote(
    general = "Notes: Statistics are computed using monthly observations. Interest rates are monthly rates, expressed in percentage points.",
    general_title = "",
    footnote_as_chunk = TRUE,
    threeparttable = TRUE
  )



# Save LaTeX table
save_kable(
  latex_table,
  file = "tables/interest_rate_descriptive_statistics.tex"
)




















# Things I'd like to plot: 
# - directed/free growth plot
# - directed share
# - inflation gap 
# - output gap 
# - commodity cycle? 

# Note: not only graphs, maybe a table too? min max for the share for example