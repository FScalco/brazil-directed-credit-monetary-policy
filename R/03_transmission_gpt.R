# ============================================================
# Smooth-transition local projections for monetary transmission
# Brazil: directed credit and monetary policy pass-through
# ============================================================
#
# Purpose:
# Estimate whether the response of credit-market outcomes to a
# monetary policy tightening differs when the directed-credit share is high.
#
# Main idea:
#
#   depvar_{t,h} =
#       alpha_h
#     + beta_h * policy_shock_t
#     + theta_h * F_high_{t-1} * policy_shock_t
#     + lambda_h * F_high_{t-1}
#     + controls and lags
#     + error_{t+h}
#
# where:
#
#   beta_h
#     = baseline response to a policy tightening
#
#   theta_h
#     = additional response when directed credit is high
#
#   beta_h + theta_h
#     = implied total response when directed credit is high
#
# The state variable is lagged by one month so that the high-directed-credit
# state is predetermined relative to the policy shock/change.
# ============================================================


# ------------------------------------------------------------
# Packages
# ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(lmtest)
library(sandwich)


# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------

df <- read_csv("../data/processed/brazil_credit_monthly_panel_with_mp_shocks.csv") %>%
  mutate(month = as.Date(month)) %>%
  arrange(month)


# ------------------------------------------------------------
# Optional: remove the acute COVID period
# ------------------------------------------------------------
#
# This mirrors your reaction-function setup.
# You can later report that including the COVID period is a robustness check.

df_no_covid <- df %>%
  filter(month < as.Date("2020-03-01") | month > as.Date("2020-12-01"))


# ------------------------------------------------------------
# Create output folders
# ------------------------------------------------------------

dir.create("figures/transmission", showWarnings = FALSE, recursive = TRUE)
dir.create("tables/transmission", showWarnings = FALSE, recursive = TRUE)


# ------------------------------------------------------------
# Helper function: create lags
# ------------------------------------------------------------
#
# This creates lag1, lag2, ..., lagK for each variable in vars.
# Example:
#   make_lags(df, c("selic_policy_rate", "credit_growth"), 6)
#
# creates:
#   selic_policy_rate_lag1, ..., selic_policy_rate_lag6
#   credit_growth_lag1, ..., credit_growth_lag6

make_lags <- function(data, vars, n_lags = 6) {
  out <- data
  
  # Keep only variables that actually exist in the dataset.
  # This avoids breaking the code if you put placeholder names below.
  vars <- vars[vars %in% names(out)]
  
  for (v in vars) {
    for (l in 1:n_lags) {
      out <- out %>%
        mutate("{v}_lag{l}" := lag(.data[[v]], l))
    }
  }
  
  out
}


# ------------------------------------------------------------
# Helper function: block wild bootstrap weights
# ------------------------------------------------------------
#
# Local projection residuals are likely serially correlated.
# A block wild bootstrap keeps signs constant within blocks.
#
# block_length = 6 is a reasonable monthly-data choice.
# You can later try 3, 6, and 12 as robustness checks.

block_wild_weights <- function(n, block_length = 6) {
  n_blocks <- ceiling(n / block_length)
  block_weights <- sample(c(-1, 1), size = n_blocks, replace = TRUE)
  rep(block_weights, each = block_length)[1:n]
}


# ------------------------------------------------------------
# Helper function: safely extract coefficients
# ------------------------------------------------------------

get_coef <- function(coefs, nm) {
  if (nm %in% names(coefs) && is.finite(coefs[[nm]])) {
    unname(coefs[[nm]])
  } else {
    NA_real_
  }
}


# ------------------------------------------------------------
# Helper function: basic bootstrap confidence interval
# ------------------------------------------------------------
#
# This matches the logic you already used:
#
#   CI_basic = 2 * beta_hat - bootstrap_quantiles
#
# It is useful because it is easy to explain and consistent with your
# reaction-function files.

basic_ci <- function(beta_hat, boot_vec) {
  
  beta_hat <- unname(as.numeric(beta_hat))
  boot_vec <- boot_vec[is.finite(boot_vec)]
  
  if (length(boot_vec) < 10) {
    return(c(lo = NA_real_, hi = NA_real_))
  }
  
  qs <- quantile(
    boot_vec,
    probs = c(0.025, 0.975),
    na.rm = TRUE,
    names = FALSE
  )
  
  out <- c(
    lo = 2 * beta_hat - qs[2],
    hi = 2 * beta_hat - qs[1]
  )
  
  out
}


# ------------------------------------------------------------
# Helper function: construct the LP dependent variable
# ------------------------------------------------------------
#
# You need slightly different dependent-variable definitions depending on
# what kind of outcome you are studying.
#
# 1. outcome_type = "growth_rate"
#
#    Use this when the outcome variable is already a monthly growth rate,
#    for example:
#
#       growth_credit_total_stock
#       growth_free_credit_stock
#       growth_directed_credit_stock
#
#    The LP dependent variable is the cumulative future growth from t to t+h:
#
#       growth_t + growth_{t+1} + ... + growth_{t+h}
#
#    If the growth rate is measured in percent, the response is approximately
#    a cumulative percentage response.
#
#
# 2. outcome_type = "level"
#
#    Use this when the outcome variable is a level, especially an interest rate:
#
#       interest_rate_free_new_operations_total
#       interest_rate_directed_new_operations_total
#       interest_rate_new_operations_total
#
#    The LP dependent variable is:
#
#       y_{t+h} - y_{t-1}
#
#    For interest rates, this gives a cumulative change in percentage points.
#
#
# 3. outcome_type = "log_level"
#
#    Use this when the outcome variable is a log credit stock:
#
#       log_credit_total_stock
#       log_free_credit_stock
#       log_directed_credit_stock
#
#    The LP dependent variable is:
#
#       100 * (log y_{t+h} - log y_{t-1})
#
#    This is an approximate cumulative percent change.

make_lp_depvar <- function(x, h, outcome_type = c("growth_rate", "level", "log_level")) {
  
  outcome_type <- match.arg(outcome_type)
  
  if (outcome_type == "growth_rate") {
    
    # Matrix with columns:
    #   lead(x, 0), lead(x, 1), ..., lead(x, h)
    #
    # rowSums(..., na.rm = FALSE) means that if any required lead is missing,
    # the dependent variable is NA.
    
    future_growth_matrix <- sapply(0:h, function(j) dplyr::lead(x, j))
    depvar <- rowSums(future_growth_matrix, na.rm = FALSE)
  }
  
  if (outcome_type == "level") {
    
    # Cumulative change in the level.
    # For interest rates, this is usually the right transformation.
    
    depvar <- dplyr::lead(x, h) - dplyr::lag(x, 1)
  }
  
  if (outcome_type == "log_level") {
    
    # Approximate cumulative percent change from t-1 to t+h.
    
    depvar <- 100 * (dplyr::lead(x, h) - dplyr::lag(x, 1))
  }
  
  depvar
}


# ============================================================
# Main function: smooth-transition transmission LP
# ============================================================

estimate_smooth_transmission_lp <- function(data = df_no_covid,
                                            outcome_var,
                                            outcome_type = c("growth_rate", "level", "log_level"),
                                            shock_var = "delta_selic",
                                            state_var = "directed_credit_share",
                                            control_vars = NULL,
                                            current_control_vars = NULL,
                                            horizons = 0:12,
                                            n_lags = 6,
                                            gamma = 1.5,
                                            state_threshold_quantile = 0.75,
                                            B_boot = 999,
                                            block_length = 6) {
  
  outcome_type <- match.arg(outcome_type)
  
  # ------------------------------------------------------------
  # Basic checks
  # ------------------------------------------------------------
  
  if (!outcome_var %in% names(data)) {
    stop(paste0("Outcome variable not found in data: ", outcome_var))
  }
  
  if (!shock_var %in% names(data)) {
    stop(paste0("Shock variable not found in data: ", shock_var))
  }
  
  if (!state_var %in% names(data)) {
    stop(paste0("State variable not found in data: ", state_var))
  }
  
  # Keep only controls that actually exist in your dataset.
  # This lets you use descriptive placeholders while drafting.
  
  control_vars <- control_vars[control_vars %in% names(data)]
  current_control_vars <- current_control_vars[current_control_vars %in% names(data)]
  
  
  # ------------------------------------------------------------
  # Variables to lag
  # ------------------------------------------------------------
  #
  # For transmission, I would usually lag:
  #
  #   - the outcome variable itself
  #   - the policy shock/change
  #   - the Selic level, if available
  #   - macro controls
  #
  # Including lags of the dependent variable helps absorb persistence.
  # Including lags of Selic helps separate the current policy impulse from
  # the policy stance.
  
  lag_vars <- unique(c(
    outcome_var,
    shock_var,
    "selic_policy_rate",
    control_vars
  ))
  
  lag_vars <- lag_vars[lag_vars %in% names(data)]
  
  
  # ------------------------------------------------------------
  # Build the smooth high-directed-credit state
  # ------------------------------------------------------------
  #
  # F_high is a smooth weight between 0 and 1.
  #
  #   F_high close to 0:
  #     directed credit is relatively low
  #
  #   F_high close to 1:
  #     directed credit is high
  #
  # The transition is centered at the chosen quantile of lagged directed credit.
  # state_threshold_quantile = 0.75 means "high directed credit" is centered
  # around the 75th percentile of the lagged directed-credit share.
  
  data_ordered <- data %>%
    arrange(month)
  
  state_lag_raw <- dplyr::lag(data_ordered[[state_var]], 1)
  state_sd <- sd(state_lag_raw, na.rm = TRUE)
  
  if (is.na(state_sd) || state_sd == 0) {
    stop("State variable has zero or undefined standard deviation after lagging.")
  }
  
  state_center <- if (is.null(state_threshold_quantile)) {
    mean(state_lag_raw, na.rm = TRUE)
  } else {
    as.numeric(
      quantile(
        state_lag_raw,
        probs = state_threshold_quantile,
        na.rm = TRUE
      )
    )
  }
  
  data_lp <- data_ordered %>%
    mutate(
      # Lagged state: predetermined directed-credit share.
      state_lag = lag(.data[[state_var]], 1),
      
      # Standardized distance from the high-state threshold.
      state_z = (state_lag - state_center) / state_sd,
      
      # Smooth high-state weight.
      F_high = 1 / (1 + exp(-gamma * state_z)),
      
      # Key nonlinear term:
      # this allows the response to the policy shock/change to differ
      # when directed credit is high.
      shock_high_extra = F_high * .data[[shock_var]]
    ) %>%
    make_lags(lag_vars, n_lags = n_lags)
  
  
  # ------------------------------------------------------------
  # Names of lag controls
  # ------------------------------------------------------------
  
  lag_control_names <- unlist(
    lapply(lag_vars, function(v) paste0(v, "_lag", 1:n_lags))
  )
  
  lag_control_names <- lag_control_names[lag_control_names %in% names(data_lp)]
  
  
  # ------------------------------------------------------------
  # Storage for horizon-by-horizon results
  # ------------------------------------------------------------
  
  results <- list()
  
  
  # ============================================================
  # Horizon loop
  # ============================================================
  
  for (h in horizons) {
    
    # ----------------------------------------------------------
    # Construct horizon-specific dependent variable
    # ----------------------------------------------------------
    
    depvar_h <- make_lp_depvar(
      x = data_lp[[outcome_var]],
      h = h,
      outcome_type = outcome_type
    )
    
    data_h <- data_lp %>%
      mutate(depvar = depvar_h)
    
    
    # ----------------------------------------------------------
    # Right-hand-side variables
    # ----------------------------------------------------------
    #
    # Important terms:
    #
    #   shock_var:
    #     baseline response to a Selic increase / monetary shock
    #
    #   shock_high_extra:
    #     additional response when directed credit is high
    #
    #   F_high:
    #     allows the average level of the outcome to differ in high states
    #
    #   lag_control_names:
    #     lagged outcome, lagged policy variables, lagged macro controls
    #
    #   current_control_vars:
    #     optional contemporaneous controls, used sparingly
    
    rhs_vars <- c(
      shock_var,
      "shock_high_extra",
      "F_high",
      current_control_vars,
      lag_control_names
    )
    
    rhs_vars <- rhs_vars[rhs_vars %in% names(data_h)]
    
    
    # ----------------------------------------------------------
    # Regression sample
    # ----------------------------------------------------------
    
    reg_data <- data_h %>%
      select(month, depvar, all_of(rhs_vars)) %>%
      drop_na()
    
    message("Outcome: ", outcome_var,
            " | Horizon ", h,
            " | usable observations: ", nrow(reg_data),
            " | regressors: ", length(rhs_vars))
    
    if (nrow(reg_data) == 0) {
      stop(paste0("No usable observations at horizon ", h))
    }
    
    
    # ----------------------------------------------------------
    # Estimate LP for this horizon
    # ----------------------------------------------------------
    
    fml <- as.formula(
      paste("depvar ~", paste(rhs_vars, collapse = " + "))
    )
    
    model <- lm(fml, data = reg_data)
    coefs <- coef(model)
    
    
    # ----------------------------------------------------------
    # Coefficients of interest
    # ----------------------------------------------------------
    #
    # beta_base:
    #   baseline effect of a one-unit increase in shock_var
    #
    # beta_extra:
    #   additional effect in high-directed-credit states
    #
    # beta_high_total:
    #   implied total effect when directed credit is high
    
    beta_base <- get_coef(coefs, shock_var)
    beta_extra <- get_coef(coefs, "shock_high_extra")
    beta_high_total <- beta_base + beta_extra
    
    
    # ----------------------------------------------------------
    # Block wild bootstrap
    # ----------------------------------------------------------
    
    n_boot <- nrow(reg_data)
    
    boot_base <- rep(NA_real_, B_boot)
    boot_extra <- rep(NA_real_, B_boot)
    boot_high_total <- rep(NA_real_, B_boot)
    
    fitted_vals <- fitted(model)
    resid_vals <- resid(model)
    
    for (b in seq_len(B_boot)) {
      
      # Draw block wild bootstrap weights.
      omega <- block_wild_weights(
        n = n_boot,
        block_length = block_length
      )
      
      # Generate bootstrap dependent variable.
      boot_data <- reg_data
      boot_data$depvar <- fitted_vals + resid_vals * omega
      
      # Re-estimate the same horizon regression.
      boot_model <- lm(fml, data = boot_data)
      boot_coefs <- coef(boot_model)
      
      # Store bootstrap coefficients.
      boot_base[b] <- get_coef(boot_coefs, shock_var)
      boot_extra[b] <- get_coef(boot_coefs, "shock_high_extra")
      boot_high_total[b] <- boot_base[b] + boot_extra[b]
    }
    
    
    # ----------------------------------------------------------
    # Bootstrap confidence intervals
    # ----------------------------------------------------------
    
    base_ci <- basic_ci(beta_base, boot_base)
    extra_ci <- basic_ci(beta_extra, boot_extra)
    high_total_ci <- basic_ci(beta_high_total, boot_high_total)
    
    
    # ----------------------------------------------------------
    # Save horizon result
    # ----------------------------------------------------------
    
    results[[as.character(h)]] <- tibble(
      outcome_var = outcome_var,
      outcome_type = outcome_type,
      shock_var = shock_var,
      state_var = state_var,
      h = h,
      
      beta_base = beta_base,
      beta_extra = beta_extra,
      beta_high_total = beta_high_total,
      
      se_base = sd(boot_base, na.rm = TRUE),
      se_extra = sd(boot_extra, na.rm = TRUE),
      se_high_total = sd(boot_high_total, na.rm = TRUE),
      
      base_lo = base_ci["lo"],
      base_hi = base_ci["hi"],
      extra_lo = extra_ci["lo"],
      extra_hi = extra_ci["hi"],
      high_total_lo = high_total_ci["lo"],
      high_total_hi = high_total_ci["hi"],
      
      n_obs = nobs(model),
      n_regressors = length(rhs_vars),
      n_parameters = length(rhs_vars) + 1,
      obs_per_parameter = nobs(model) / (length(rhs_vars) + 1),
      
      gamma = gamma,
      state_center = state_center,
      state_threshold_quantile = ifelse(
        is.null(state_threshold_quantile),
        NA_real_,
        state_threshold_quantile
      )
    )
  }
  
  bind_rows(results)
}




# ============================================================
# Example 1: pass-through to free-credit growth
# ============================================================
#
# Replace the variable names with your actual names.
#
# outcome_type = "growth_rate" because the dependent variable is already
# a monthly growth rate.

transmission_free_growth <- estimate_smooth_transmission_lp(
  data = df_no_covid,
  outcome_var = "growth_free_credit_stock",
  outcome_type = "growth_rate",
  
  # Use your true shock variable here if you have one.
  # For example:
  #   shock_var = "monetary_policy_shock"
  #
  # If not, delta_selic is okay as a policy-change proxy.
  shock_var = "delta_selic",
  
  state_var = "directed_credit_share",
  
  control_vars = c(
    # "ibc_output_gap",
    # "focus_inflation_gap",
    # "growth_icbr_commodities"
    # # "exchange_rate_log_change"
  ),
  
  current_control_vars = c(
    # "growth_icbr_commodities"
  ),
  
  horizons = 0:12,
  n_lags = 6,
  gamma = 1.5,
  state_threshold_quantile = 0.75,
  B_boot = 999,
  block_length = 6
)


# ============================================================
# Example 2: pass-through to total credit growth
# ============================================================

transmission_total_growth <- estimate_smooth_transmission_lp(
  data = df_no_covid,
  outcome_var = "growth_credit_total_stock",
  outcome_type = "growth_rate",
  shock_var = "delta_selic",
  state_var = "directed_credit_share",
  control_vars = c(
    # "ibc_output_gap",
    # "focus_inflation_gap",
    # "growth_icbr_commodities"
    # # "exchange_rate_log_change"
  ),
  current_control_vars = c(
    # "growth_icbr_commodities"
  ),
  horizons = 0:12,
  n_lags = 6,
  gamma = 1.5,
  state_threshold_quantile = 0.75,
  B_boot = 999,
  block_length = 6
)


# ============================================================
# Example 3: pass-through to free-credit interest rates
# ============================================================
#
# outcome_type = "level" because the dependent variable is an interest-rate
# level. The response is then:
#
#   interest_rate_{t+h} - interest_rate_{t-1}
#
# This is analogous to your cumulative Selic-response construction.

transmission_free_rate <- estimate_smooth_transmission_lp(
  data = df_no_covid,
  outcome_var = "interest_rate_free_new_operations_total",
  outcome_type = "level",
  shock_var = "delta_selic",
  state_var = "directed_credit_share",
  control_vars = c(
    # "ibc_output_gap",
    # "focus_inflation_gap",
    # "growth_icbr_commodities"
    # # "exchange_rate_log_change"
  ),
  current_control_vars = c(
    # "growth_icbr_commodities"
  ),
  horizons = 0:12,
  n_lags = 6,
  gamma = 1.5,
  state_threshold_quantile = 0.75,
  B_boot = 999,
  block_length = 6
)


# ============================================================
# Example 4: pass-through to directed-credit interest rates
# ============================================================

transmission_directed_rate <- estimate_smooth_transmission_lp(
  data = df_no_covid,
  outcome_var = "interest_rate_directed_new_operations_total",
  outcome_type = "level",
  shock_var = "delta_selic",
  state_var = "directed_credit_share",
  control_vars = c(
    # "ibc_output_gap",
    # "focus_inflation_gap",
    # "growth_icbr_commodities"
    # # "exchange_rate_log_change"
  ),
  current_control_vars = c(
    # "growth_icbr_commodities"
  ),
  horizons = 0:12,
  n_lags = 6,
  gamma = 1.5,
  state_threshold_quantile = 0.75,
  B_boot = 999,
  block_length = 6
)





# ============================================================
# Plot helper: baseline vs high-directed-credit response
# ============================================================

plot_transmission_baseline_vs_high <- function(lp_results,
                                               plot_title,
                                               plot_subtitle,
                                               y_label,
                                               output_file) {
  
  plot_data <- bind_rows(
    lp_results %>%
      transmute(
        h = h,
        response = "Baseline response",
        estimate = beta_base,
        lo = base_lo,
        hi = base_hi
      ),
    
    lp_results %>%
      transmute(
        h = h,
        response = "Total response when directed credit is high",
        estimate = beta_high_total,
        lo = high_total_lo,
        hi = high_total_hi
      )
  )
  
  p <- ggplot(plot_data, aes(x = h, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.7) +
    facet_wrap(~ response) +
    scale_x_continuous(breaks = sort(unique(plot_data$h))) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Horizon (months)",
      y = y_label
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(
    filename = output_file,
    plot = p,
    width = 9,
    height = 5,
    dpi = 300
  )
  
  p
}


# ============================================================
# Plot helper: additional high-state response only
# ============================================================
#
# This is often the cleanest figure for your paper.
# It directly answers:
#
#   Does the pass-through response differ when directed credit is high?

plot_transmission_extra <- function(lp_results,
                                    plot_title,
                                    plot_subtitle,
                                    y_label,
                                    output_file) {
  
  p <- ggplot(lp_results, aes(x = h, y = beta_extra)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_ribbon(aes(ymin = extra_lo, ymax = extra_hi), alpha = 0.20) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.7) +
    scale_x_continuous(breaks = sort(unique(lp_results$h))) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Horizon (months)",
      y = y_label
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(
    filename = output_file,
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  p
}



# ------------------------------------------------------------
# Free-credit growth: baseline vs high state
# ------------------------------------------------------------

p_free_growth_levels <- plot_transmission_baseline_vs_high(
  lp_results = transmission_free_growth,
  plot_title = "Monetary policy pass-through to free-credit growth",
  plot_subtitle = "Smooth-transition local projection; baseline and high directed-credit state",
  y_label = "Cumulative credit-growth response",
  output_file = "figures/transmission/free_credit_growth_baseline_vs_high.png"
)


# ------------------------------------------------------------
# Free-credit growth: additional high-state response
# ------------------------------------------------------------

p_free_growth_extra <- plot_transmission_extra(
  lp_results = transmission_free_growth,
  plot_title = "Additional response of free-credit growth",
  plot_subtitle = "Smooth-transition local projection; high directed-credit state",
  y_label = "Additional cumulative growth response",
  output_file = "figures/transmission/free_credit_growth_extra_high_state.png"
)


# ------------------------------------------------------------
# Free-credit interest rate: baseline vs high state
# ------------------------------------------------------------

p_free_rate_levels <- plot_transmission_baseline_vs_high(
  lp_results = transmission_free_rate,
  plot_title = "Monetary policy pass-through to free-credit interest rates",
  plot_subtitle = "Smooth-transition local projection; baseline and high directed-credit state",
  y_label = "Cumulative interest-rate response, p.p.",
  output_file = "figures/transmission/free_credit_rate_baseline_vs_high.png"
)


# ------------------------------------------------------------
# Free-credit interest rate: additional high-state response
# ------------------------------------------------------------

p_free_rate_extra <- plot_transmission_extra(
  lp_results = transmission_free_rate,
  plot_title = "Additional response of free-credit interest rates",
  plot_subtitle = "Smooth-transition local projection; high directed-credit state",
  y_label = "Additional interest-rate response, p.p.",
  output_file = "figures/transmission/free_credit_rate_extra_high_state.png"
)







# ============================================================
# Degrees-of-freedom and parsimony audit for transmission LPs
# ============================================================
#
# Purpose:
# Check whether the smooth-transition LP is too heavy relative to
# the available sample size.
#
# This does NOT test causality.
# It checks whether the model is using too many parameters for the
# number of usable observations.
#
# Main diagnostics:
#
#   n_obs:
#     Number of usable observations at each horizon.
#
#   n_parameters:
#     Number of estimated parameters, including the intercept.
#
#   residual_df:
#     Remaining degrees of freedom after estimating the model.
#
#   obs_per_parameter:
#     Number of observations per estimated parameter.
#
#   mean_leverage:
#     Average leverage. In OLS, this is approximately K / N.
#     Higher values mean the model is using up more of the sample.
#
#   parsimony_flag:
#     Simple warning label.
# ============================================================


library(tidyverse)


# ------------------------------------------------------------
# Helper: create lags
# ------------------------------------------------------------

make_lags <- function(data, vars, n_lags = 6) {
  out <- data
  
  vars <- vars[vars %in% names(out)]
  
  for (v in vars) {
    for (l in 1:n_lags) {
      out <- out %>%
        mutate("{v}_lag{l}" := lag(.data[[v]], l))
    }
  }
  
  out
}


# ------------------------------------------------------------
# Helper: construct LP dependent variable
# ------------------------------------------------------------

make_lp_depvar <- function(x, h, outcome_type = c("growth_rate", "level", "log_level")) {
  
  outcome_type <- match.arg(outcome_type)
  
  if (outcome_type == "growth_rate") {
    future_growth_matrix <- sapply(0:h, function(j) dplyr::lead(x, j))
    depvar <- rowSums(future_growth_matrix, na.rm = FALSE)
  }
  
  if (outcome_type == "level") {
    depvar <- dplyr::lead(x, h) - dplyr::lag(x, 1)
  }
  
  if (outcome_type == "log_level") {
    depvar <- 100 * (dplyr::lead(x, h) - dplyr::lag(x, 1))
  }
  
  depvar
}


# ------------------------------------------------------------
# Main audit function
# ------------------------------------------------------------

audit_transmission_parsimony <- function(data,
                                         outcome_var,
                                         outcome_type = c("growth_rate", "level", "log_level"),
                                         shock_var = "delta_selic",
                                         state_var = "directed_credit_share",
                                         control_vars = NULL,
                                         current_control_vars = NULL,
                                         horizons = 0:12,
                                         n_lags = 6,
                                         gamma = 1.5,
                                         state_threshold_quantile = 0.75) {
  
  outcome_type <- match.arg(outcome_type)
  
  # Keep only variables that actually exist.
  control_vars <- control_vars[control_vars %in% names(data)]
  current_control_vars <- current_control_vars[current_control_vars %in% names(data)]
  
  # These variables will receive lags.
  lag_vars <- unique(c(
    outcome_var,
    shock_var,
    "selic_policy_rate",
    control_vars
  ))
  
  lag_vars <- lag_vars[lag_vars %in% names(data)]
  
  # ----------------------------------------------------------
  # Create smooth high-directed-credit state
  # ----------------------------------------------------------
  
  data_ordered <- data %>%
    arrange(month)
  
  state_lag_raw <- lag(data_ordered[[state_var]], 1)
  state_sd <- sd(state_lag_raw, na.rm = TRUE)
  
  state_center <- as.numeric(
    quantile(
      state_lag_raw,
      probs = state_threshold_quantile,
      na.rm = TRUE
    )
  )
  
  data_lp <- data_ordered %>%
    mutate(
      state_lag = lag(.data[[state_var]], 1),
      state_z = (state_lag - state_center) / state_sd,
      F_high = 1 / (1 + exp(-gamma * state_z)),
      shock_high_extra = F_high * .data[[shock_var]]
    ) %>%
    make_lags(lag_vars, n_lags = n_lags)
  
  # Names of lagged controls.
  lag_control_names <- unlist(
    lapply(lag_vars, function(v) paste0(v, "_lag", 1:n_lags))
  )
  
  lag_control_names <- lag_control_names[lag_control_names %in% names(data_lp)]
  
  audit_results <- list()
  
  # ----------------------------------------------------------
  # Horizon loop
  # ----------------------------------------------------------
  
  for (h in horizons) {
    
    depvar_h <- make_lp_depvar(
      x = data_lp[[outcome_var]],
      h = h,
      outcome_type = outcome_type
    )
    
    data_h <- data_lp %>%
      mutate(depvar = depvar_h)
    
    rhs_vars <- c(
      shock_var,
      "shock_high_extra",
      "F_high",
      current_control_vars,
      lag_control_names
    )
    
    rhs_vars <- rhs_vars[rhs_vars %in% names(data_h)]
    
    reg_data <- data_h %>%
      select(month, depvar, all_of(rhs_vars)) %>%
      drop_na()
    
    # If no usable observations remain, store missing diagnostics.
    if (nrow(reg_data) == 0) {
      audit_results[[as.character(h)]] <- tibble(
        outcome_var = outcome_var,
        outcome_type = outcome_type,
        h = h,
        n_lags = n_lags,
        n_obs = 0,
        n_parameters = NA_real_,
        model_rank = NA_real_,
        residual_df = NA_real_,
        obs_per_parameter = NA_real_,
        mean_leverage = NA_real_,
        max_leverage = NA_real_,
        parsimony_flag = "No usable observations"
      )
      
      next
    }
    
    fml <- as.formula(
      paste("depvar ~", paste(rhs_vars, collapse = " + "))
    )
    
    model <- lm(fml, data = reg_data)
    
    # Model matrix gives the actual number of columns including the intercept.
    X <- model.matrix(model)
    
    n_obs <- nrow(X)
    n_parameters <- ncol(X)
    model_rank <- qr(X)$rank
    residual_df <- df.residual(model)
    obs_per_parameter <- n_obs / model_rank
    
    # Leverage diagnostics.
    leverages <- hatvalues(model)
    mean_leverage <- mean(leverages, na.rm = TRUE)
    max_leverage <- max(leverages, na.rm = TRUE)
    
    parsimony_flag <- case_when(
      obs_per_parameter < 5 ~ "Too heavy",
      obs_per_parameter < 10 ~ "Fragile",
      TRUE ~ "Comfortable"
    )
    
    audit_results[[as.character(h)]] <- tibble(
      outcome_var = outcome_var,
      outcome_type = outcome_type,
      h = h,
      n_lags = n_lags,
      n_obs = n_obs,
      n_parameters = n_parameters,
      model_rank = model_rank,
      residual_df = residual_df,
      obs_per_parameter = obs_per_parameter,
      mean_leverage = mean_leverage,
      max_leverage = max_leverage,
      parsimony_flag = parsimony_flag
    )
  }
  
  bind_rows(audit_results)
}




audit_free_rate_lean <- audit_transmission_parsimony(
  data = df_no_covid,
  outcome_var = "interest_rate_free_new_operations_total",
  outcome_type = "level",
  shock_var = "delta_selic",
  state_var = "directed_credit_share",
  
  # Lean specification:
  # no output gap, no inflation gap, no external macro controls.
  control_vars = c(),
  current_control_vars = c(),
  
  horizons = 0:12,
  n_lags = 6,
  gamma = 1.5,
  state_threshold_quantile = 0.75
)

audit_free_rate_lean


audit_free_rate_macro <- audit_transmission_parsimony(
  data = df_no_covid,
  outcome_var = "interest_rate_free_new_operations_total",
  outcome_type = "level",
  shock_var = "delta_selic",
  state_var = "directed_credit_share",
  
  # Richer specification:
  # adds macro controls as lagged variables.
  control_vars = c(
    "ibc_output_gap",
    "focus_inflation_gap",
    "growth_icbr_commodities",
    "exchange_rate_log_change"
  ),
  
  current_control_vars = c(),
  
  horizons = 0:12,
  n_lags = 6,
  gamma = 1.5,
  state_threshold_quantile = 0.75
)

audit_free_rate_macro

parsimony_comparison <- bind_rows(
  audit_free_rate_lean %>%
    mutate(specification = "Lean"),
  
  audit_free_rate_macro %>%
    mutate(specification = "Macro controls")
) %>%
  select(
    specification,
    h,
    n_lags,
    n_obs,
    n_parameters,
    residual_df,
    obs_per_parameter,
    mean_leverage,
    max_leverage,
    parsimony_flag
  )

parsimony_comparison


parsimony_summary <- parsimony_comparison %>%
  group_by(specification) %>%
  summarise(
    min_n_obs = min(n_obs, na.rm = TRUE),
    max_n_parameters = max(n_parameters, na.rm = TRUE),
    min_residual_df = min(residual_df, na.rm = TRUE),
    min_obs_per_parameter = min(obs_per_parameter, na.rm = TRUE),
    max_mean_leverage = max(mean_leverage, na.rm = TRUE),
    max_leverage = max(max_leverage, na.rm = TRUE),
    worst_flag = case_when(
      any(parsimony_flag == "Too heavy") ~ "Too heavy",
      any(parsimony_flag == "Fragile") ~ "Fragile",
      TRUE ~ "Comfortable"
    ),
    .groups = "drop"
  )

parsimony_summary



# ============================================================
# Lag-length parsimony comparison
# ============================================================
#
# This compares 3, 6, 9, and 12 lags.
# For your interest-rate sample, this will show very clearly whether
# 6 or 12 lags are too expensive in degrees-of-freedom terms.

lag_grid <- c(3, 6, 9, 12)

audit_lag_grid_free_rate <- map_dfr(lag_grid, function(L) {
  
  audit_transmission_parsimony(
    data = df_no_covid,
    outcome_var = "interest_rate_free_new_operations_total",
    outcome_type = "level",
    shock_var = "delta_selic",
    state_var = "directed_credit_share",
    control_vars = c(),
    current_control_vars = c(),
    horizons = 0:12,
    n_lags = L,
    gamma = 1.5,
    state_threshold_quantile = 0.75
  )
  
}) %>%
  mutate(specification = paste0(n_lags, " lags, lean"))


lag_grid_summary_free_rate <- audit_lag_grid_free_rate %>%
  group_by(specification, n_lags) %>%
  summarise(
    min_n_obs = min(n_obs, na.rm = TRUE),
    max_n_parameters = max(n_parameters, na.rm = TRUE),
    min_residual_df = min(residual_df, na.rm = TRUE),
    min_obs_per_parameter = min(obs_per_parameter, na.rm = TRUE),
    max_leverage = max(max_leverage, na.rm = TRUE),
    worst_flag = case_when(
      any(parsimony_flag == "Too heavy") ~ "Too heavy",
      any(parsimony_flag == "Fragile") ~ "Fragile",
      TRUE ~ "Comfortable"
    ),
    .groups = "drop"
  )

lag_grid_summary_free_rate



ggplot(audit_lag_grid_free_rate,
       aes(x = h, y = obs_per_parameter, group = factor(n_lags))) +
  geom_hline(yintercept = 5, linetype = "dashed", linewidth = 0.4) +
  geom_hline(yintercept = 10, linetype = "dotted", linewidth = 0.4) +
  geom_line(aes(linetype = factor(n_lags)), linewidth = 0.9) +
  geom_point(aes(shape = factor(n_lags)), size = 1.7) +
  scale_x_continuous(breaks = 0:12) +
  labs(
    title = "Degrees-of-freedom audit for the transmission model",
    subtitle = "Observations per estimated parameter across lag choices",
    x = "Horizon (months)",
    y = "Observations per parameter",
    linetype = "Number of lags",
    shape = "Number of lags"
  ) +
  theme_minimal(base_size = 12)