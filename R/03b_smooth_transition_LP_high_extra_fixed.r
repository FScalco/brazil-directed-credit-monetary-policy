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
    calendar_month = month(month)
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
                                      gamma = 3,
                                      state_threshold_quantile = 0.75,
                                      B_boot = 999,
                                      block_length = 6) {
  
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
  
  data_ordered <- data %>% arrange(month)
  state_lag_raw <- dplyr::lag(data_ordered[[state_var]], 1)
  state_sd <- sd(state_lag_raw, na.rm = TRUE)
  
  if (is.na(state_sd) || state_sd == 0) {
    stop("State variable has zero or undefined standard deviation after lagging.")
  }
  
  # Center the smooth transition on a genuinely high directed-credit-share value.
  # Use state_threshold_quantile = NULL to recover the old mean-centered transition.
  state_center <- if (is.null(state_threshold_quantile)) {
    mean(state_lag_raw, na.rm = TRUE)
  } else {
    as.numeric(quantile(state_lag_raw, probs = state_threshold_quantile, na.rm = TRUE))
  }
  
  data_lp <- data_ordered %>%
    mutate(
      # State is predetermined.
      state_lag = lag(.data[[state_var]], 1),
      
      # Positive values mean the directed-credit share is above the chosen high threshold.
      state_z = (state_lag - state_center) / state_sd,
      
      # Smooth high-directed-credit-share weight.
      # This goes toward 0 below the threshold and toward 1 above it.
      F_high = 1 / (1 + exp(-gamma * state_z)),
      
      # Baseline Taylor-rule variables plus high-state extra responses.
      output_gap_high_extra = F_high * .data[[output_gap_var]],
      inflation_gap_high_extra = F_high * .data[[inflation_gap_var]]
    ) %>%
    make_lags(lag_vars, n_lags = n_lags)
  
  lag_control_names <- unlist(
    lapply(lag_vars, function(v) paste0(v, "_lag", 1:n_lags))
  )
  
  get_coef <- function(coefs, nm) {
    if (nm %in% names(coefs) && !is.na(coefs[[nm]])) {
      unname(coefs[[nm]])
    } else {
      NA_real_
    }
  }
  
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
      2 * beta_hat - qs[2],
      2 * beta_hat - qs[1]
    )
    
    names(out) <- c("lo", "hi")
    out
  }
  
  results <- list()
  
  for (h in horizons) {
    
    data_h <- data_lp %>%
      mutate(
        # Cumulative future change in Selic relative to the pre-horizon policy rate.
        depvar = lead(.data[["selic_policy_rate"]], h) - lag(.data[["selic_policy_rate"]], 1)
        # Alternative if you want the simple h-step-ahead outcome instead:
        # depvar = lead(.data[[policy_outcome]], h)
      )
    
    rhs_vars <- c(
      "F_high",                         # state intercept shift, not a low-regime slope
      output_gap_var,                    # baseline output-gap response
      inflation_gap_var,                 # baseline inflation-gap response
      "output_gap_high_extra",          # additional output-gap response when high
      "inflation_gap_high_extra",       # additional inflation-gap response when high
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
    
    # Important: keep the ordinary intercept here.
    # The old symmetric specification needed 0 + F_low + F_high.
    # This baseline-plus-extra specification does not.
    fml <- as.formula(
      paste("depvar ~", paste(rhs_vars, collapse = " + "))
    )
    
    model <- lm(fml, data = reg_data)
    coefs <- coef(model)
    
    # Original estimates: baseline response, extra high-state response,
    # and implied total high-state response.
    beta_x_base <- get_coef(coefs, output_gap_var)
    beta_x_extra <- get_coef(coefs, "output_gap_high_extra")
    beta_x_high_total <- beta_x_base + beta_x_extra
    
    beta_pi_base <- get_coef(coefs, inflation_gap_var)
    beta_pi_extra <- get_coef(coefs, "inflation_gap_high_extra")
    beta_pi_high_total <- beta_pi_base + beta_pi_extra
    
    n_boot <- nrow(reg_data)
    
    boot_x_base <- rep(NA_real_, B_boot)
    boot_x_extra <- rep(NA_real_, B_boot)
    boot_x_high_total <- rep(NA_real_, B_boot)
    
    boot_pi_base <- rep(NA_real_, B_boot)
    boot_pi_extra <- rep(NA_real_, B_boot)
    boot_pi_high_total <- rep(NA_real_, B_boot)
    
    fitted_vals <- fitted(model)
    resid_vals <- resid(model)
    
    for (b in seq_len(B_boot)) {
      omega <- block_wild_weights(n_boot, block_length = block_length)
      
      boot_data <- reg_data
      boot_data$depvar <- fitted_vals + resid_vals * omega
      
      boot_model <- lm(fml, data = boot_data)
      boot_coefs <- coef(boot_model)
      
      boot_x_base[b] <- get_coef(boot_coefs, output_gap_var)
      boot_x_extra[b] <- get_coef(boot_coefs, "output_gap_high_extra")
      boot_x_high_total[b] <- boot_x_base[b] + boot_x_extra[b]
      
      boot_pi_base[b] <- get_coef(boot_coefs, inflation_gap_var)
      boot_pi_extra[b] <- get_coef(boot_coefs, "inflation_gap_high_extra")
      boot_pi_high_total[b] <- boot_pi_base[b] + boot_pi_extra[b]
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
      
      n_obs = nobs(model),
      state_center = state_center,
      state_threshold_quantile = ifelse(is.null(state_threshold_quantile), NA_real_, state_threshold_quantile),
      gamma = gamma
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
    #"exchange_rate_log_change",
    "growth_credit_total_stock",
    "growth_icbr_commodities"
  ),
  current_control_vars = c(
    #"exchange_rate_log_change",
    "growth_icbr_commodities"
  ),
  horizons = 0:12,
  n_lags = 6,
  gamma = 1.5,
  state_threshold_quantile = 0.75,
  B_boot = 999,
  block_length = 6
)


dir.create("figures", showWarnings = FALSE, recursive = TRUE)

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
