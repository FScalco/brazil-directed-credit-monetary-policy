# ============================================================
# Smooth-transition local projections for monetary pass-through
# ============================================================

# Packages
library(tidyverse)
library(lubridate)
library(sandwich)
library(lmtest)
library(broom)

# ------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------

df <- read_csv("../data/processed/brazil_credit_monthly_panel_with_mp_shocks.csv") %>%
  mutate(month = as.Date(month)) %>%
  arrange(month)

# Quick check
glimpse(df)

# ------------------------------------------------------------
# 2. Choose variables
# ------------------------------------------------------------

# Main outcome options:
# "interest_rate_directed_new_operations_total"
# "interest_rate_free_new_operations_total"
# "growth_directed_credit_stock"
# "growth_free_credit_stock"

outcome_var <- "interest_rate_free_new_operations_total"

# Preferred shock:
# If you have monetary policy surprises, use them.
# Otherwise use delta_selic, but interpret less causally.
# shock_var <- "mp_shock_pp_zero"
shock_var <- "delta_selic"

state_var <- "directed_credit_share"

# Controls.
# Keep this relatively small because monthly macro samples are not huge.
control_vars <- c(
  "selic_policy_rate",
  "ipca_12m",
  "industrial_output_general",
  "exchange_rate_log_change",
  "unemployment_rate_pnadc"
)

# Remove controls that are not present or are all missing
control_vars <- control_vars[
  control_vars %in% names(df) &
    sapply(df[control_vars], function(x) !all(is.na(x)))
]

control_vars


# ------------------------------------------------------------
# 3. Create smooth transition function
# ------------------------------------------------------------

# The state should be predetermined, so use lagged directed-credit share.
# Standardizing makes gamma easier to interpret.
#
# gamma controls how sharp the transition is:
#   gamma = 1   very smooth
#   gamma = 2   moderately smooth
#   gamma = 5+  close to a hard threshold
#
# I would start with gamma = 1.5 or 2.

gamma <- 1.5

df_st <- df %>%
  mutate(
    state_lag = lag(.data[[state_var]], 1),
    state_z = as.numeric(scale(state_lag)),
    
    # Smooth high-regime weight
    F_high = 1 / (1 + exp(-gamma * state_z)),
    
    # Smooth low-regime weight
    F_low = 1 - F_high
  )

# Plot the regime weight through time
ggplot(df_st, aes(x = month, y = F_high)) +
  geom_line() +
  labs(
    title = "Smooth high-directed-credit-share regime weight",
    subtitle = paste0("Logistic transition function, gamma = ", gamma),
    x = NULL,
    y = "High-regime weight"
  ) +
  theme_minimal()



# ------------------------------------------------------------
# 4. Helper function to create lags
# ------------------------------------------------------------

make_lags <- function(data, vars, n_lags = 6) {
  out <- data
  
  for (v in vars) {
    for (l in 1:n_lags) {
      out <- out %>%
        mutate("{v}_lag{l}" := lag(.data[[v]], l))
    }
  }
  
  out
}





# ------------------------------------------------------------
# 5. Smooth-transition LP estimator
# ------------------------------------------------------------

estimate_smooth_lp <- function(data,
                               outcome_var,
                               shock_var,
                               control_vars = NULL,
                               horizons = 0:12,
                               n_lags = 6,
                               gamma = 1.5,
                               cumulative = TRUE) {
  
  # Variables to lag as controls.
  # Include the outcome and the shock dynamics.
  lag_vars <- unique(c(outcome_var, shock_var, control_vars))
  
  data_lp <- data %>%
    arrange(month) %>%
    mutate(
      state_lag = lag(.data[[state_var]], 1),
      state_z = as.numeric(scale(state_lag)),
      F_high = 1 / (1 + exp(-gamma * state_z)),
      F_low = 1 - F_high
    ) %>%
    make_lags(lag_vars, n_lags = n_lags)
  
  # Names of lagged controls
  lag_control_names <- unlist(
    lapply(lag_vars, function(v) paste0(v, "_lag", 1:n_lags))
  )
  
  results <- list()
  
  for (h in horizons) {
    
    # LP dependent variable.
    #
    # If cumulative = TRUE:
    #   y_{t+h} - y_{t-1}
    # This gives the cumulative change in the lending rate.
    #
    # If cumulative = FALSE:
    #   y_{t+h}
    # This gives the level response.
    
    data_h <- data_lp %>%
      mutate(
        y_lead = lead(.data[[outcome_var]], h),
        y_lag1 = lag(.data[[outcome_var]], 1),
        depvar = ifelse(cumulative, y_lead - y_lag1, y_lead)
      )
    
    # Smooth-transition regressors.
    #
    # We include:
    #   F_low and F_high as regime-specific intercepts
    #   shock interacted with F_low and F_high
    #   lag controls interacted with F_low and F_high
    #
    # This allows the whole dynamic system to differ smoothly by regime,
    # not only the shock coefficient.
    
    data_h <- data_h %>%
      mutate(
        shock_low = F_low * .data[[shock_var]],
        shock_high = F_high * .data[[shock_var]]
      )
    
    for (x in lag_control_names) {
      data_h[[paste0(x, "_low")]] <- data_h$F_low * data_h[[x]]
      data_h[[paste0(x, "_high")]] <- data_h$F_high * data_h[[x]]
    }
    
    rhs_vars <- c(
      "F_low",
      "F_high",
      "shock_low",
      "shock_high",
      paste0(lag_control_names, "_low"),
      paste0(lag_control_names, "_high")
    )
    
    fml <- as.formula(
      paste("depvar ~ 0 +", paste(rhs_vars, collapse = " + "))
    )
    
    reg_data <- data_h %>%
      select(month, depvar, all_of(rhs_vars)) %>%
      drop_na()
    
    model <- lm(fml, data = reg_data)
    
    # Newey-West / HAC standard errors.
    # LP residuals are serially correlated because y_{t+h} overlaps across t.
    # A common choice is lag = h + 1.
    
    vcov_hac <- NeweyWest(
      model,
      lag = h + 1,
      prewhite = FALSE,
      adjust = TRUE
    )
    
    coefs <- coef(model)
    V <- vcov_hac
    
    beta_low <- coefs["shock_low"]
    beta_high <- coefs["shock_high"]
    beta_diff <- beta_high - beta_low
    
    se_low <- sqrt(V["shock_low", "shock_low"])
    se_high <- sqrt(V["shock_high", "shock_high"])
    
    se_diff <- sqrt(
      V["shock_high", "shock_high"] +
        V["shock_low", "shock_low"] -
        2 * V["shock_high", "shock_low"]
    )
    
    results[[as.character(h)]] <- tibble(
      h = h,
      beta_low = beta_low,
      beta_high = beta_high,
      beta_diff = beta_diff,
      se_low = se_low,
      se_high = se_high,
      se_diff = se_diff,
      n_obs = nobs(model)
    )
  }
  
  bind_rows(results) %>%
    mutate(
      low_lo = beta_low - 1.96 * se_low,
      low_hi = beta_low + 1.96 * se_low,
      high_lo = beta_high - 1.96 * se_high,
      high_hi = beta_high + 1.96 * se_high,
      diff_lo = beta_diff - 1.96 * se_diff,
      diff_hi = beta_diff + 1.96 * se_diff
    )
}



# ------------------------------------------------------------
# 6. Estimate model
# ------------------------------------------------------------

lp_st <- estimate_smooth_lp(
  data = df_st,
  outcome_var = outcome_var,
  shock_var = shock_var,
  control_vars = control_vars,
  horizons = 0:12,
  n_lags = 6,
  gamma = gamma,
  cumulative = TRUE
)

lp_st