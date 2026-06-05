library(tidyverse)
library(lubridate)
library(readr)

# Paths
panel_path <- "../data/processed/brazil_credit_monthly_panel.csv"
shock_path <- "../data/raw/MP_shocks.csv"

# Load monthly panel
panel <- read_csv(panel_path, show_col_types = FALSE) %>%
  mutate(month = as.Date(month))

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
panel_mp <- panel %>%
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