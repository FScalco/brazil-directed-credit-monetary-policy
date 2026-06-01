# ============================================================
# 01_check_data.R
# Inspect cleaned monthly panel
# ============================================================


# install.packages(c("tidyverse", "lubridate", "janitor", "scales"))

library(tidyverse)
library(lubridate)
library(janitor)
library(scales)

panel_file <- "../data/processed/brazil_credit_monthly_panel.csv"

df <- read_csv(panel_file, show_col_types = FALSE) |>
  clean_names() |>
  mutate(month = ymd(month))

# ------------------------------------------------------------
# 1. Basic structure
# ------------------------------------------------------------

glimpse(df)

df |>
  summarise(
    first_month = min(month, na.rm = TRUE),
    last_month = max(month, na.rm = TRUE),
    n_months = n(),
    n_variables = ncol(df)
  ) |>
  print()

# ------------------------------------------------------------
# 2. Missing values
# ------------------------------------------------------------

missing_table <- df |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_count"
  ) |>
  arrange(desc(missing_count))

print(missing_table, n = Inf)

# ------------------------------------------------------------
# 3. First and last non-missing date by variable
# ------------------------------------------------------------

coverage_table <- df |>
  pivot_longer(
    cols = -month,
    names_to = "variable",
    values_to = "value"
  ) |>
  filter(!is.na(value)) |>
  group_by(variable) |>
  summarise(
    first_nonmissing = min(month),
    last_nonmissing = max(month),
    n_obs = n(),
    .groups = "drop"
  ) |>
  arrange(first_nonmissing, variable)

print(coverage_table, n = Inf)

# ------------------------------------------------------------
# 4. Accounting identity check
# total credit should be close to free credit + directed credit
# ------------------------------------------------------------

if (all(c("credit_total_stock", "free_credit_stock", "directed_credit_stock") %in% names(df))) {
  identity_check <- df |>
    mutate(
      credit_sum = free_credit_stock + directed_credit_stock,
      credit_gap = credit_total_stock - credit_sum,
      credit_gap_pct = credit_gap / credit_total_stock
    ) |>
    summarise(
      mean_gap_pct = mean(credit_gap_pct, na.rm = TRUE),
      max_abs_gap_pct = max(abs(credit_gap_pct), na.rm = TRUE)
    )

  print(identity_check)
}

# ------------------------------------------------------------
# 5. Directed and free credit shares
# ------------------------------------------------------------

if ("directed_credit_share" %in% names(df)) {
  df |>
    summarise(
      min_directed_share = min(directed_credit_share, na.rm = TRUE),
      max_directed_share = max(directed_credit_share, na.rm = TRUE),
      mean_directed_share = mean(directed_credit_share, na.rm = TRUE)
    ) |>
    print()

  ggplot(df, aes(x = month, y = directed_credit_share)) +
    geom_line() +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Directed credit share",
      subtitle = "Earmarked credit outstanding / total credit outstanding",
      x = NULL,
      y = "Share of total credit"
    )
}

# ------------------------------------------------------------
# 6. Credit stocks
# ------------------------------------------------------------

credit_stock_vars <- c(
  "credit_total_stock",
  "free_credit_stock",
  "directed_credit_stock"
)

if (all(credit_stock_vars %in% names(df))) {
  df |>
    select(month, all_of(credit_stock_vars)) |>
    pivot_longer(
      cols = -month,
      names_to = "series",
      values_to = "value"
    ) |>
    ggplot(aes(x = month, y = value, color = series)) +
    geom_line() +
    labs(
      title = "Credit outstanding",
      x = NULL,
      y = "Level"
    )
}

# ------------------------------------------------------------
# 7. New-operation interest rates
# ------------------------------------------------------------

rate_vars <- c(
  "interest_rate_free_new_operations_total",
  "interest_rate_directed_new_operations_total"
)

if (all(rate_vars %in% names(df))) {
  df |>
    select(month, all_of(rate_vars)) |>
    pivot_longer(
      cols = -month,
      names_to = "series",
      values_to = "value"
    ) |>
    ggplot(aes(x = month, y = value, color = series)) +
    geom_line() +
    labs(
      title = "New-operation interest rates",
      subtitle = "Free vs directed credit",
      x = NULL,
      y = "% per month"
    )
}

# ------------------------------------------------------------
# 8. Monetary policy and macro controls
# ------------------------------------------------------------

macro_vars <- c(
  "selic_policy_rate",
  "selic_annual_daily",
  "delta_selic",
  "ipca",
  "industrial_output_general",
  "exchange_rate_usd_sale_avg",
  "exchange_rate_log_change"
)

macro_vars_existing <- intersect(macro_vars, names(df))

if (length(macro_vars_existing) > 0) {
  df |>
    select(month, all_of(macro_vars_existing)) |>
    pivot_longer(
      cols = -month,
      names_to = "series",
      values_to = "value"
    ) |>
    ggplot(aes(x = month, y = value)) +
    geom_line() +
    facet_wrap(~ series, scales = "free_y") +
    labs(
      title = "Macro controls",
      x = NULL,
      y = NULL
    )
}