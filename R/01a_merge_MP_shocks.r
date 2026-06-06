## I merge the MP shock series, and also create the macro gap variables


library(tidyverse)
library(lubridate)

# Paths
panel_path <- "../data/processed/brazil_credit_monthly_panel.csv"
shock_path <- "../data/raw/MP_shocks.csv"

# Load monthly panel
panel <- read_csv(panel_path, show_col_types = FALSE) %>%
  mutate(month = as.Date(month)) %>%
  arrange(month)


df <- panel %>%
  mutate(
    t = row_number(),
    calendar_month = month(month),
    
    # Output gap 1: BCB's economic activity proxy
    log_ibc_br = log(ibc_br_sa),
    
    # Output gap 2: industrial output
    log_ip = log(industrial_output_general),
    
    # Inflation gap: inflation minus target
    inflation_gap = ipca_12m - inflation_target
  )


# Output gap proxy 1:
# residual from log IBC index on trend
# Multiplied by 100, so it's approx. a pct deviation from trend
output_gap_model = lm(
  log_ibc_br ~ t,
  data = df,
  na.action = na.exclude
)



# Output gap proxy 2:
# residual from log industrial production on trend and seasonality.
# Multiplied by 100, so it is approximately percentage deviation from trend.
output_gap_model_alt <- lm(
  log_ip ~ t + factor(calendar_month),
  data = df,
  na.action = na.exclude
)

df <- df %>%
  mutate(
    #IBC-br as output gap
    log_ibc_trend = fitted(output_gap_model),
    ibc_br_trend = exp(log_ibc_trend),
    output_gap = resid(output_gap_model) * 100,
    #Industrial output as output gap
    log_ip_trend = fitted(output_gap_model_alt),
    industrial_output_trend = exp(log_ip_trend),
    output_gap_alt = resid(output_gap_model_alt) * 100
  ) %>%
  select(-t, -calendar_month, -log_ip, -log_ip_trend, -log_ibc_br ,-log_ibc_trend)







# Load shocks and keep Brazil
mp_brazil_monthly <- read_csv(shock_path, show_col_types = FALSE) %>%
  filter(Country == "Brazil") %>%
  mutate(
    market_Date = mdy(market_Date),
    orig_Date   = mdy(orig_Date),
    month       = floor_date(market_Date, "month"),
    
    # mps appears to be in basis points; convert to percentage points too
    mp_shock_bp = mps,
    mp_shock_pp = mps / 100
  ) %>%
  group_by(month) %>%
  summarise(
    mp_shock_bp = sum(mp_shock_bp, na.rm = TRUE),
    mp_shock_pp = sum(mp_shock_pp, na.rm = TRUE),
    n_mp_meetings = n(),
    emergency_meeting = max(emergency_meeting, na.rm = TRUE),
    .groups = "drop"
  )

# Merge into panel
panel_mp <- df %>%
  left_join(mp_brazil_monthly, by = "month") %>%
  mutate(
    # Set missing shock months to zero if you want a monthly series with no-event months = 0.
    # Keep the original NA version if you only want event-month observations.
    mp_shock_bp_zero = replace_na(mp_shock_bp, 0),
    mp_shock_pp_zero = replace_na(mp_shock_pp, 0),
    n_mp_meetings = replace_na(n_mp_meetings, 0)
  )

# Quick checks
panel_mp %>%
  select(month, selic_policy_rate, delta_selic, mp_shock_bp, mp_shock_pp, n_mp_meetings) %>%
  filter(!is.na(mp_shock_bp)) %>%
  print(n = 30)

# Save merged panel
write_csv(panel_mp, "../data/processed/brazil_credit_monthly_panel_with_mp_shocks.csv")