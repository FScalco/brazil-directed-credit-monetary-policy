library(tidyverse)
library(lubridate)
library(sandwich)
library(lmtest)

df <- read_csv("../data/processed/brazil_credit_monthly_panel_with_mp_shocks.csv") %>%
  mutate(month = as.Date(month)) %>%
  arrange(month)

# ------------------------------------------------------------
# Construct macro gaps
# ------------------------------------------------------------

df <- df %>%
  mutate(
    t = row_number(),
    calendar_month = month(month),
    log_ip = log(industrial_output_general),
    
    # Inflation gap: inflation minus target
    inflation_gap = ipca_12m - inflation_target
  )

# Output gap proxy:
# residual from log industrial production on trend and seasonality.
# Multiplied by 100, so it is approximately percentage deviation from trend.
df <- df %>%
  mutate(
    t = row_number(),
    calendar_month = month(month),
    log_ip = log(industrial_output_general),
    inflation_gap = ipca_12m - inflation_target
  )

output_gap_model <- lm(
  log_ip ~ t + factor(calendar_month),
  data = df,
  na.action = na.exclude
)

df <- df %>%
  mutate(
    output_gap = resid(output_gap_model) * 100
  )

# # HP-filtered industrial production (Optional)
# library(mFilter)
# 
# # Create HP-filtered output gap
# df_hp <- df %>%
#   filter(!is.na(log_ip)) %>%
#   arrange(month)
# 
# ip_ts <- ts(df_hp$log_ip, frequency = 12)
# 
# hp_ip <- hpfilter(ip_ts, freq = 129600)
# 
# df_hp <- df_hp %>%
#   mutate(
#     output_gap_hp = as.numeric(hp_ip$cycle) * 100
#   )
# 
# df <- df %>%
#   left_join(
#     df_hp %>% select(month, output_gap_hp),
#     by = "month"
#   )
# 
# # Rename / overwrite the variable used everywhere else
# df <- df %>%
#   mutate(
#     output_gap = output_gap_hp
#   )
# 


output_gap_var <- "output_gap"

# Check created variables
df %>%
  select(month, selic_policy_rate, delta_selic, output_gap, inflation_gap, directed_credit_share, icbr_commodities) %>%
  tail()


# ------------------------------------------------------------
# Helper: create lags
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
# Helper: block wild bootstrap weights
# ------------------------------------------------------------

block_wild_weights <- function(n, block_length) {
  n_blocks <- ceiling(n / block_length)
  block_weights <- sample(c(-1, 1), size = n_blocks, replace = TRUE)
  rep(block_weights, each = block_length)[1:n]
}



################### Taking out COVID period from sample

df_no_covid <- df %>%
  filter(month < as.Date("2020-03-01") | month > as.Date("2020-12-01"))

# ------------------------------------------------------------
# Smooth-transition Taylor-rule LP
# ------------------------------------------------------------

estimate_smooth_policy_lp <- function(data = df_no_covid,
                                      policy_outcome = "delta_selic",
                                      state_var = "directed_credit_share",
                                      output_gap_var = "output_gap",
                                      inflation_gap_var = "inflation_gap",
                                      control_vars = NULL,
                                      current_control_vars = NULL,
                                      horizons = 0:12,
                                      n_lags = 6,
                                      gamma = 3) {
  
  control_vars <- control_vars[control_vars %in% names(data)]
  current_control_vars <- current_control_vars[current_control_vars %in% names(data)]
  
  # Lags included as ordinary controls.
  # These help absorb policy persistence and macro dynamics.
  lag_vars <- unique(c(
    "selic_policy_rate",
    output_gap_var,
    inflation_gap_var,
    control_vars
  ))
  
  data_lp <- data %>%
    arrange(month) %>%
    mutate(
      # State must be predetermined
      state_lag = lag(.data[[state_var]], 1),
      
      # Standardize state variable so gamma is interpretable
      state_z = as.numeric(scale(state_lag)),
      
      # Smooth high-directed-credit-share weight
      F_high = 1 / (1 + exp(-gamma * state_z)),
      
      # Additional response when directed credit share is high
      output_gap_high_extra = F_high * .data[[output_gap_var]],
      inflation_gap_high_extra = F_high * .data[[inflation_gap_var]]
    ) %>%
    make_lags(lag_vars, n_lags = n_lags)
  
  
  lag_control_names <- unlist(
    lapply(lag_vars, function(v) paste0(v, "_lag", 1:n_lags))
  )
  
  results <- list()
  
  for (h in horizons) {
    
    data_h <- data_lp %>%
      mutate(
        # Future policy response.
        # If policy_outcome = delta_selic, this is future change in Selic.
        # If policy_outcome = selic_policy_rate, this is future Selic level.
        # depvar = lead(.data[[policy_outcome]], h)
        depvar = lead(selic_policy_rate,h) - lag(selic_policy_rate,1)
        
      )
    
    rhs_vars <- c(
      "F_high",
      output_gap_var,
      inflation_gap_var,
      "output_gap_high_extra",
      "inflation_gap_high_extra",
      lag_control_names,
      current_control_vars
    )
    
    reg_data <- data_h %>%
      select(month, depvar, all_of(rhs_vars)) %>%
      drop_na()
    
    message("Horizon ", h, ": ", nrow(reg_data), " usable observations")
    
    if (nrow(reg_data) == 0) {
      missing_check <- data_h %>%
        select(depvar, all_of(rhs_vars)) %>%
        summarise(across(everything(), ~ sum(!is.na(.x)))) %>%
        pivot_longer(
          everything(),
          names_to = "variable",
          values_to = "nonmissing"
        ) %>%
        arrange(nonmissing)
      
      print(missing_check)
      stop(paste0("No usable observations at horizon ", h))
    }
    
    fml <- as.formula(
      paste("depvar ~ 0 +", paste(rhs_vars, collapse = " + "))
    )
    
    model <- lm(fml, data = reg_data)
    
#     # HAC standard errors.
#     # For LPs, residuals are serially correlated at longer horizons.
#     vcov_hac <- NeweyWest(
#       model,
#       lag = h + 1,
#       prewhite = FALSE,
#       adjust = TRUE
#     )
#     
#     coefs <- coef(model)
#     V <- vcov_hac
#     
#     # Output-gap effects
#     beta_x_low <- coefs["output_gap_low"]
#     beta_x_high <- coefs["output_gap_high"]
#     beta_x_diff <- beta_x_high - beta_x_low
#     
#     se_x_low <- sqrt(V["output_gap_low", "output_gap_low"])
#     se_x_high <- sqrt(V["output_gap_high", "output_gap_high"])
#     se_x_diff <- sqrt(
#       V["output_gap_high", "output_gap_high"] +
#         V["output_gap_low", "output_gap_low"] -
#         2 * V["output_gap_high", "output_gap_low"]
#     )
#     
#     # Inflation-gap effects
#     beta_pi_low <- coefs["inflation_gap_low"]
#     beta_pi_high <- coefs["inflation_gap_high"]
#     beta_pi_diff <- beta_pi_high - beta_pi_low
#     
#     se_pi_low <- sqrt(V["inflation_gap_low", "inflation_gap_low"])
#     se_pi_high <- sqrt(V["inflation_gap_high", "inflation_gap_high"])
#     se_pi_diff <- sqrt(
#       V["inflation_gap_high", "inflation_gap_high"] +
#         V["inflation_gap_low", "inflation_gap_low"] -
#         2 * V["inflation_gap_high", "inflation_gap_low"]
#     )
#     
#     results[[as.character(h)]] <- tibble(
#       h = h,
#       beta_x_low = beta_x_low,
#       beta_x_high = beta_x_high,
#       beta_x_diff = beta_x_diff,
#       se_x_low = se_x_low,
#       se_x_high = se_x_high,
#       se_x_diff = se_x_diff,
#       beta_pi_low = beta_pi_low,
#       beta_pi_high = beta_pi_high,
#       beta_pi_diff = beta_pi_diff,
#       se_pi_low = se_pi_low,
#       se_pi_high = se_pi_high,
#       se_pi_diff = se_pi_diff,
#       n_obs = nobs(model)
#     )
#   }
#   
#   bind_rows(results) %>%
#     mutate(
#       x_low_lo = beta_x_low - 1.96 * se_x_low,
#       x_low_hi = beta_x_low + 1.96 * se_x_low,
#       x_high_lo = beta_x_high - 1.96 * se_x_high,
#       x_high_hi = beta_x_high + 1.96 * se_x_high,
#       x_diff_lo = beta_x_diff - 1.96 * se_x_diff,
#       x_diff_hi = beta_x_diff + 1.96 * se_x_diff,
#       
#       pi_low_lo = beta_pi_low - 1.96 * se_pi_low,
#       pi_low_hi = beta_pi_low + 1.96 * se_pi_low,
#       pi_high_lo = beta_pi_high - 1.96 * se_pi_high,
#       pi_high_hi = beta_pi_high + 1.96 * se_pi_high,
#       pi_diff_lo = beta_pi_diff - 1.96 * se_pi_diff,
#       pi_diff_hi = beta_pi_diff + 1.96 * se_pi_diff
#     )
# }

    
    # ------------------------------------------------------------
    # Wild bootstrap inference: basic bootstrap bands
    # ------------------------------------------------------------
    
    coefs <- coef(model)
    
    # Original estimates
    beta_x_base <- coefs[output_gap_var]
    beta_x_extra <- coefs["output_gap_high_extra"]
    beta_x_high_total <- beta_x_base + beta_x_extra
    
    beta_pi_base <- coefs[inflation_gap_var]
    beta_pi_extra <- coefs["inflation_gap_high_extra"]
    beta_pi_high_total <- beta_pi_base + beta_pi_extra
    
    # Bootstrap setup
    B_boot <- 999
    n_boot <- nrow(reg_data)
    
    boot_x_base <- rep(NA_real_, B_boot)
    boot_x_extra <- rep(NA_real_, B_boot)
    boot_x_high_total <- rep(NA_real_, B_boot)
    
    boot_pi_base <- rep(NA_real_, B_boot)
    boot_pi_extra <- rep(NA_real_, B_boot)
    boot_pi_high_total <- rep(NA_real_, B_boot)
    
    fitted_vals <- fitted(model)
    resid_vals <- resid(model)
    
    for (b in 1:B_boot) {
      
      omega <- block_wild_weights(n_boot, block_length = 6)
      
      boot_data <- reg_data
      boot_data$depvar <- fitted_vals + resid_vals * omega
      
      boot_model <- lm(fml, data = boot_data)
      boot_coefs <- coef(boot_model)
      
      boot_x_base[b] <- boot_coefs[output_gap_var]
      boot_x_extra[b] <- boot_coefs["output_gap_high_extra"]
      boot_x_high_total[b] <- boot_x_base[b] + boot_x_extra[b]
      
      boot_pi_base[b] <- boot_coefs[inflation_gap_var]
      boot_pi_extra[b] <- boot_coefs["inflation_gap_high_extra"]
      boot_pi_high_total[b] <- boot_pi_base[b] + boot_pi_extra[b]
    }
    
    # Basic bootstrap confidence interval helper
    basic_ci <- function(beta_hat, boot_vec) {
      q025 <- quantile(boot_vec, 0.025, na.rm = TRUE)
      q975 <- quantile(boot_vec, 0.975, na.rm = TRUE)
      
      c(
        lo = 2 * beta_hat - q975,
        hi = 2 * beta_hat - q025
      )
    }
    
    x_base_ci <- basic_ci(beta_x_base, boot_x_base)
    x_extra_ci <- basic_ci(beta_x_extra, boot_x_extra)
    x_high_total_ci <- basic_ci(beta_x_high_total, boot_x_high_total)
    
    pi_base_ci <- basic_ci(beta_pi_base, boot_pi_base)
    pi_extra_ci <- basic_ci(beta_pi_extra, boot_pi_extra)
    pi_high_total_ci <- basic_ci(beta_pi_high_total, boot_pi_high_total)
    
    results[[as.character(h)]] <- tibble(
      h = h,
      
      beta_x_base = beta_x_base,
      beta_x_extra = beta_x_extra,
      beta_x_high_total = beta_x_high_total,
      se_x_base = sd(boot_x_base, na.rm = TRUE),
      se_x_extra = sd(boot_x_extra, na.rm = TRUE),
      se_x_high_total = sd(boot_x_high_total, na.rm = TRUE),
      x_base_lo = x_base_ci["lo"],
      x_base_hi = x_base_ci["hi"],
      x_extra_lo = x_extra_ci["lo"],
      x_extra_hi = x_extra_ci["hi"],
      x_high_total_lo = x_high_total_ci["lo"],
      x_high_total_hi = x_high_total_ci["hi"],
      
      beta_pi_base = beta_pi_base,
      beta_pi_extra = beta_pi_extra,
      beta_pi_high_total = beta_pi_high_total,
      se_pi_base = sd(boot_pi_base, na.rm = TRUE),
      se_pi_extra = sd(boot_pi_extra, na.rm = TRUE),
      se_pi_high_total = sd(boot_pi_high_total, na.rm = TRUE),
      pi_base_lo = pi_base_ci["lo"],
      pi_base_hi = pi_base_ci["hi"],
      pi_extra_lo = pi_extra_ci["lo"],
      pi_extra_hi = pi_extra_ci["hi"],
      pi_high_total_lo = pi_high_total_ci["lo"],
      pi_high_total_hi = pi_high_total_ci["hi"],
      
      n_obs = nobs(model)
    )
    
  }
    bind_rows(results)
    
}


# Estimating the model 
policy_lp <- estimate_smooth_policy_lp(
  data = df_no_covid,
  state_var = "directed_credit_share",
  output_gap_var = "output_gap",
  inflation_gap_var = "inflation_gap",
  control_vars = c(
    "exchange_rate_log_change",
    "growth_credit_total_stock",
    "growth_icbr_commodities"
  ),
  current_control_vars = c(
    "exchange_rate_log_change",
    "growth_icbr_commodities"
  ),
  horizons = 0:12,
  n_lags = 6,
  gamma = 3
)


# ------------------------------------------------------------
# Plotting: output-gap effects
# ------------------------------------------------------------

plot_output_gap_effects <- bind_rows(
  policy_lp %>%
    transmute(
      h = h,
      response = "Baseline Taylor-like response",
      estimate = beta_x_base,
      lo = x_base_lo,
      hi = x_base_hi
    ),
  
  policy_lp %>%
    transmute(
      h = h,
      response = "Total response when directed credit is high",
      estimate = beta_x_high_total,
      lo = x_high_total_lo,
      hi = x_high_total_hi
    )
)

p_output_levels <- ggplot(plot_output_gap_effects, aes(x = h, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ response) +
  labs(
    title = "Policy reaction to the output gap",
    subtitle = "Baseline response and implied total response when directed credit is high",
    x = "Horizon",
    y = "Marginal effect of output gap on cumulative Selic change"
  ) +
  theme_minimal()

ggsave(
  "figures/smooth_policy_reaction_output_baseline_vs_high.png",
  p_output_levels,
  width = 9,
  height = 5,
  dpi = 300
)


p_output_extra <- ggplot(policy_lp, aes(x = h, y = beta_x_extra)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = x_extra_lo, ymax = x_extra_hi), alpha = 0.25) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  labs(
    title = "Does directed credit amplify the policy reaction to the output gap?",
    subtitle = "Smooth-transition LP: additional response when directed-credit share is high",
    x = "Horizon",
    y = "Additional output-gap coefficient"
  ) +
  theme_minimal()

ggsave(
  "figures/smooth_extra_output.png",
  p_output_extra,
  width = 9,
  height = 5,
  dpi = 300
)


# ------------------------------------------------------------
# Plotting: inflation-gap effects
# ------------------------------------------------------------

plot_inflation_gap_effects <- bind_rows(
  policy_lp %>%
    transmute(
      h = h,
      response = "Baseline Taylor-like response",
      estimate = beta_pi_base,
      lo = pi_base_lo,
      hi = pi_base_hi
    ),
  
  policy_lp %>%
    transmute(
      h = h,
      response = "Total response when directed credit is high",
      estimate = beta_pi_high_total,
      lo = pi_high_total_lo,
      hi = pi_high_total_hi
    )
)

p_inflation_levels <- ggplot(plot_inflation_gap_effects, aes(x = h, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ response) +
  labs(
    title = "Policy reaction to the inflation gap",
    subtitle = "Baseline response and implied total response when directed credit is high",
    x = "Horizon",
    y = "Marginal effect of inflation gap on cumulative Selic change"
  ) +
  theme_minimal()

ggsave(
  "figures/smooth_policy_reaction_inflation_baseline_vs_high.png",
  p_inflation_levels,
  width = 9,
  height = 5,
  dpi = 300
)


p_inflation_extra <- ggplot(policy_lp, aes(x = h, y = beta_pi_extra)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = pi_extra_lo, ymax = pi_extra_hi), alpha = 0.25) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  labs(
    title = "Does directed credit amplify the policy reaction to inflation?",
    subtitle = "Smooth-transition LP: additional response when directed-credit share is high",
    x = "Horizon",
    y = "Additional inflation-gap coefficient"
  ) +
  theme_minimal()

ggsave(
  "figures/smooth_extra_inflation.png",
  p_inflation_extra,
  width = 9,
  height = 5,
  dpi = 300
)
