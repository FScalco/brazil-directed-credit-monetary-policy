# ============================================================
# Linear transmission local projections
# Whole-sample benchmark response to Selic changes
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



###### Adding this optional control of credit to GDP ratio 

df <- df %>%
  arrange(month) %>%
  mutate(
    credit_gdp_l1 = lag(credit_total_stock_to_gdp, 1),
    credit_gdp_l1_dm = credit_gdp_l1 - mean(credit_gdp_l1, na.rm = TRUE)
  )


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
# Helper: create lags
# ------------------------------------------------------------

make_lags <- function(data, vars, n_lags = 3) {
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
#
# growth_rate:
#   cumulative future growth:
#   growth_t + growth_{t+1} + ... + growth_{t+h}
#
# level:
#   cumulative change in level:
#   y_{t+h} - y_{t-1}
#
# For interest rates, use outcome_type = "level".

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
# Helper: block wild bootstrap weights
# ------------------------------------------------------------

block_wild_weights <- function(n, block_length = 6) {
  n_blocks <- ceiling(n / block_length)
  block_weights <- sample(c(-1, 1), size = n_blocks, replace = TRUE)
  rep(block_weights, each = block_length)[1:n]
}


# ------------------------------------------------------------
# Helper: safe coefficient extraction
# ------------------------------------------------------------

get_coef <- function(coefs, nm) {
  if (nm %in% names(coefs) && is.finite(coefs[[nm]])) {
    unname(coefs[[nm]])
  } else {
    NA_real_
  }
}


# ------------------------------------------------------------
# Helper: basic bootstrap confidence interval
# ------------------------------------------------------------

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
  
  c(
    lo = 2 * beta_hat - qs[2],
    hi = 2 * beta_hat - qs[1]
  )
}


# ============================================================
# Main function: linear transmission LP
# ============================================================

estimate_linear_transmission_lp <- function(data,
                                            outcome_var,
                                            outcome_type = c("growth_rate", "level", "log_level"),
                                            shock_var = "delta_selic",
                                            control_vars = NULL,
                                            current_control_vars = NULL,
                                            horizons = 0:12,
                                            n_lags = 3,
                                            B_boot = 999,
                                            block_length = 6) {
  
  outcome_type <- match.arg(outcome_type)
  
  if (!outcome_var %in% names(data)) {
    stop(paste0("Outcome variable not found: ", outcome_var))
  }
  
  if (!shock_var %in% names(data)) {
    stop(paste0("Shock variable not found: ", shock_var))
  }
  
  control_vars <- control_vars[control_vars %in% names(data)]
  current_control_vars <- current_control_vars[current_control_vars %in% names(data)]
  
  # Dynamic controls only.
  # These are not macro controls; they capture persistence in credit outcomes
  # and in the monetary-policy cycle.
  lag_vars <- unique(c(
    outcome_var,
    # shock_var,
    "selic_policy_rate",
    control_vars
  ))
  
  lag_vars <- lag_vars[lag_vars %in% names(data)]
  
  data_lp <- data %>%
    arrange(month) %>%
    make_lags(lag_vars, n_lags = n_lags)
  
  lag_control_names <- unlist(
    lapply(lag_vars, function(v) paste0(v, "_lag", 1:n_lags))
  )
  
  lag_control_names <- lag_control_names[lag_control_names %in% names(data_lp)]
  
  results <- list()
  
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
      current_control_vars,
      lag_control_names
    )
    
    rhs_vars <- rhs_vars[rhs_vars %in% names(data_h)]
    
    reg_data <- data_h %>%
      select(month, depvar, all_of(rhs_vars)) %>%
      drop_na()
    
    message(
      "Outcome: ", outcome_var,
      " | Horizon ", h,
      " | usable observations: ", nrow(reg_data),
      " | regressors: ", length(rhs_vars)
    )
    
    fml <- as.formula(
      paste("depvar ~", paste(rhs_vars, collapse = " + "))
    )
    
    model <- lm(fml, data = reg_data)
    coefs <- coef(model)
    
    beta <- get_coef(coefs, shock_var)
    
    fitted_vals <- fitted(model)
    resid_vals <- resid(model)
    n_boot <- nrow(reg_data)
    
    boot_beta <- rep(NA_real_, B_boot)
    
    for (b in seq_len(B_boot)) {
      
      omega <- block_wild_weights(
        n = n_boot,
        block_length = block_length
      )
      
      boot_data <- reg_data
      boot_data$depvar <- fitted_vals + resid_vals * omega
      
      boot_model <- lm(fml, data = boot_data)
      boot_beta[b] <- get_coef(coef(boot_model), shock_var)
    }
    
    beta_ci <- basic_ci(beta, boot_beta)
    
    results[[as.character(h)]] <- tibble(
      outcome_var = outcome_var,
      outcome_type = outcome_type,
      shock_var = shock_var,
      h = h,
      beta = beta,
      se = sd(boot_beta, na.rm = TRUE),
      lo = beta_ci["lo"],
      hi = beta_ci["hi"],
      n_obs = nobs(model),
      n_regressors = length(rhs_vars),
      n_parameters = length(rhs_vars) + 1,
      obs_per_parameter = nobs(model) / (length(rhs_vars) + 1)
    )
  }
  
  bind_rows(results)
}


# ============================================================
# Linear LPs: credit growth outcomes
# ============================================================

linear_total_growth <- estimate_linear_transmission_lp(
  data = df_no_covid,
  outcome_var = "growth_credit_total_stock",
  outcome_type = "growth_rate",
  shock_var = "delta_selic",
  control_vars = c(),
  current_control_vars = c(),
  horizons = 0:12,
  n_lags = 6,
  B_boot = 999,
  block_length = 6
)

linear_free_growth <- estimate_linear_transmission_lp(
  data = df_no_covid,
  outcome_var = "growth_free_credit_stock",
  outcome_type = "growth_rate",
  shock_var = "delta_selic",
  control_vars = c(),
  current_control_vars = c(),
  horizons = 0:12,
  n_lags = 6,
  B_boot = 999,
  block_length = 6
)

linear_directed_growth <- estimate_linear_transmission_lp(
  data = df_no_covid,
  outcome_var = "growth_directed_credit_stock",
  outcome_type = "growth_rate",
  shock_var = "delta_selic",
  control_vars = c(),
  current_control_vars = c(),
  horizons = 0:12,
  n_lags = 6,
  B_boot = 999,
  block_length = 6
)



# ============================================================
# Linear LPs: credit interest-rate outcomes
# ============================================================

linear_total_rate <- estimate_linear_transmission_lp(
  data = df_no_covid,
  outcome_var = "interest_rate_new_operations_total",
  outcome_type = "level",
  shock_var = "delta_selic",
  control_vars = c(),
  current_control_vars = c(),
  horizons = 0:12,
  n_lags = 6,
  B_boot = 999,
  block_length = 6
)

linear_free_rate <- estimate_linear_transmission_lp(
  data = df_no_covid,
  outcome_var = "interest_rate_free_new_operations_total",
  outcome_type = "level",
  shock_var = "delta_selic",
  control_vars = c(),
  current_control_vars = c(),
  horizons = 0:12,
  n_lags = 6,
  B_boot = 999,
  block_length = 6
)

linear_directed_rate <- estimate_linear_transmission_lp(
  data = df_no_covid,
  outcome_var = "interest_rate_directed_new_operations_total",
  outcome_type = "level",
  shock_var = "delta_selic",
  control_vars = c(),
  current_control_vars = c(),
  horizons = 0:12,
  n_lags = 6,
  B_boot = 999,
  block_length = 6
)



# ============================================================
# Plot helper for linear LPs
# ============================================================

plot_linear_transmission <- function(lp_results,
                                     plot_title,
                                     plot_subtitle,
                                     y_label,
                                     output_file) {
  
  p <- ggplot(lp_results, aes(x = h, y = beta)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20) +
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


# Growth rate plots

p_linear_total_growth <- plot_linear_transmission(
  lp_results = linear_total_growth,
  plot_title = "Monetary policy pass-through to total credit growth",
  plot_subtitle = "Linear local projection; whole-sample response",
  y_label = "Cumulative credit-growth response",
  output_file = "figures/transmission/linear_total_credit_growth.png"
)

p_linear_free_growth <- plot_linear_transmission(
  lp_results = linear_free_growth,
  plot_title = "Monetary policy pass-through to free-credit growth",
  plot_subtitle = "Linear local projection; whole-sample response",
  y_label = "Cumulative credit-growth response",
  output_file = "figures/transmission/linear_free_credit_growth.png"
)

p_linear_directed_growth <- plot_linear_transmission(
  lp_results = linear_directed_growth,
  plot_title = "Monetary policy pass-through to directed-credit growth",
  plot_subtitle = "Linear local projection; whole-sample response",
  y_label = "Cumulative credit-growth response",
  output_file = "figures/transmission/linear_directed_credit_growth.png"
)


# Interest rate plots

p_linear_total_rate <- plot_linear_transmission(
  lp_results = linear_total_rate,
  plot_title = "Monetary policy pass-through to total credit interest rates",
  plot_subtitle = "Linear local projection; whole-sample response",
  y_label = "Cumulative interest-rate response, p.p.",
  output_file = "figures/transmission/linear_total_credit_rate.png"
)

p_linear_free_rate <- plot_linear_transmission(
  lp_results = linear_free_rate,
  plot_title = "Monetary policy pass-through to free-credit interest rates",
  plot_subtitle = "Linear local projection; whole-sample response",
  y_label = "Cumulative interest-rate response, p.p.",
  output_file = "figures/transmission/linear_free_credit_rate.png"
)

p_linear_directed_rate <- plot_linear_transmission(
  lp_results = linear_directed_rate,
  plot_title = "Monetary policy pass-through to directed-credit interest rates",
  plot_subtitle = "Linear local projection; whole-sample response",
  y_label = "Cumulative interest-rate response, p.p.",
  output_file = "figures/transmission/linear_directed_credit_rate.png"
)