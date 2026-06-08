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
    
    
    
#    log_ip = log(industrial_output_general)
    
    # Inflation gap: inflation minus target
    #inflation_gap = ipca_12m - inflation_target
  )

# Output gap proxy:
# residual from log industrial production on trend and seasonality.
# Multiplied by 100, so it is approximately percentage deviation from trend.

# output_gap_model <- lm(
#   log_ip ~ t + factor(calendar_month),
#   data = df,
#   na.action = na.exclude
# )
# 
# df <- df %>%
#   mutate(
#     output_gap = resid(output_gap_model) * 100
#   )



# output_gap_var <- "output_gap"
# 
# # Check created variables
# df %>%
#   select(month, selic_policy_rate, delta_selic, output_gap, inflation_gap, directed_credit_share, icbr_commodities) %>%
#   tail()


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


################################################################################
###### Linear Policy LP (just changed the function to show cumulative response)



# df <- df %>%
#   mutate(
#     output_gap_w = pmin(
#       pmax(output_gap, quantile(output_gap, 0.01, na.rm = TRUE)),
#       quantile(output_gap, 0.99, na.rm = TRUE)
#     )
#   )

df_no_covid <- df %>%
  filter(month < as.Date("2020-03-01") | month > as.Date("2020-12-01"))

estimate_linear_policy_lp <- function(data = df_no_covid,
                                      output_gap_var = "ibc_output_gap",
                                      inflation_gap_var = "focus_inflation_gap",
                                      control_vars = NULL,
                                      current_control_vars = NULL,
                                      horizons = 0:12,
                                      n_lags = 6) 
  
  
#   # ------------------------------------------------------------
# # HAC standard errors
# # ------------------------------------------------------------
# {
#   
#   control_vars <- control_vars[control_vars %in% names(data)]
#   current_control_vars <- current_control_vars[current_control_vars %in% names(data)]
#   
#   lag_vars <- unique(c(
#     "selic_policy_rate",
#     output_gap_var,
#     inflation_gap_var,
#     control_vars
#   ))
#   
#   data_lp <- data %>%
#     arrange(month) %>%
#     make_lags(lag_vars, n_lags = n_lags)
#   
#   lag_control_names <- unlist(
#     lapply(lag_vars, function(v) paste0(v, "_lag", 1:n_lags))
#   )
#   
#   results <- list()
#   
#   for (h in horizons) {
#     
#     data_h <- data_lp %>%
#       mutate(
#         #             depvar = lead(.data[[policy_outcome]], h)
#         depvar = lead(selic_policy_rate,h) - lag(selic_policy_rate,1)
#       )
#     
#     rhs_vars <- c(
#       output_gap_var,
#       inflation_gap_var,
#       current_control_vars,
#       lag_control_names
#     )
#     
#     reg_data <- data_h %>%
#       select(month, depvar, all_of(rhs_vars)) %>%
#       drop_na()
#     
#     fml <- as.formula(
#       paste("depvar ~", paste(rhs_vars, collapse = " + "))
#     )
#     
#     model <- lm(fml, data = reg_data)
#     
#     
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
#     results[[as.character(h)]] <- tibble(
#       h = h,
#       beta_x = coefs[output_gap_var],
#       beta_pi = coefs[inflation_gap_var],
#       se_x = sqrt(V[output_gap_var, output_gap_var]),
#       se_pi = sqrt(V[inflation_gap_var, inflation_gap_var]),
#       n_obs = nobs(model)
#     )
#   }
#   
#   bind_rows(results) %>%
#     mutate(
#       x_lo = beta_x - 1.96 * se_x,
#       x_hi = beta_x + 1.96 * se_x,
#       pi_lo = beta_pi - 1.96 * se_pi,
#       pi_hi = beta_pi + 1.96 * se_pi
#     )
# }


# ------------------------------------------------------------
# Wild bootstrap standard errors
# ------------------------------------------------------------

{


  control_vars <- control_vars[control_vars %in% names(data)]
  current_control_vars <- current_control_vars[current_control_vars %in% names(data)]

  lag_vars <- unique(c(
    "selic_policy_rate",
    output_gap_var,
    inflation_gap_var,
    control_vars
  ))

  data_lp <- data %>%
    arrange(month) %>%
    make_lags(lag_vars, n_lags = n_lags)

  lag_control_names <- unlist(
    lapply(lag_vars, function(v) paste0(v, "_lag", 1:n_lags))
  )

  results <- list()

  for (h in horizons) {

    data_h <- data_lp %>%
      mutate(
#        depvar = lead(.data[[policy_outcome]], h)
         depvar = lead(selic_policy_rate,h) - lag(selic_policy_rate,1)
        )

    rhs_vars <- c(
      output_gap_var,
      inflation_gap_var,
      current_control_vars,
      lag_control_names
    )

    reg_data <- data_h %>%
      select(month, depvar, all_of(rhs_vars)) %>%
      drop_na()

    fml <- as.formula(
      paste("depvar ~", paste(rhs_vars, collapse = " + "))
    )

    model <- lm(fml, data = reg_data)

    # ------------------------------------------------------------
    # Wild bootstrap inference: basic bootstrap bands
    # ------------------------------------------------------------

    coefs <- coef(model)

    beta_x_hat <- coefs[output_gap_var]
    beta_pi_hat <- coefs[inflation_gap_var]

    B_boot <- 999
    n_boot <- nrow(reg_data)

    boot_beta_x <- rep(NA_real_, B_boot)
    boot_beta_pi <- rep(NA_real_, B_boot)

    fitted_vals <- fitted(model)
    resid_vals <- resid(model)

    for (b in 1:B_boot) {

      # Rademacher wild bootstrap weights: +1 or -1
      omega <- block_wild_weights(n_boot, block_length = 6)
      
      boot_data <- reg_data
      boot_data$depvar <- fitted_vals + resid_vals * omega

      boot_model <- lm(fml, data = boot_data)
      boot_coefs <- coef(boot_model)

      boot_beta_x[b] <- boot_coefs[output_gap_var]
      boot_beta_pi[b] <- boot_coefs[inflation_gap_var]
    }

    # Bootstrap standard errors, useful to report
    se_x <- sd(boot_beta_x, na.rm = TRUE)
    se_pi <- sd(boot_beta_pi, na.rm = TRUE)

    # Basic bootstrap confidence intervals
    x_q025 <- quantile(boot_beta_x, 0.025, na.rm = TRUE)
    x_q975 <- quantile(boot_beta_x, 0.975, na.rm = TRUE)

    pi_q025 <- quantile(boot_beta_pi, 0.025, na.rm = TRUE)
    pi_q975 <- quantile(boot_beta_pi, 0.975, na.rm = TRUE)

    results[[as.character(h)]] <- tibble(
      h = h,
      beta_x = beta_x_hat,
      beta_pi = beta_pi_hat,
      se_x = se_x,
      se_pi = se_pi,
      x_lo = 2 * beta_x_hat - x_q975,
      x_hi = 2 * beta_x_hat - x_q025,
      pi_lo = 2 * beta_pi_hat - pi_q975,
      pi_hi = 2 * beta_pi_hat - pi_q025,
      n_obs = nobs(model)
    )

  }
  bind_rows(results)
}







########################################## Running it 

policy_lp_linear <- estimate_linear_policy_lp(
  data = df_no_covid,
  output_gap_var = "ibc_output_gap",
  inflation_gap_var = "focus_inflation_gap",
  control_vars = c(
#    "exchange_rate_log_change",
#    "growth_credit_total_stock",
#    "growth_free_credit_stock",
    "growth_icbr_commodities"
  ),
  current_control_vars = c(
#    "exchange_rate_log_change",
    "growth_icbr_commodities"
  ),
  horizons = 0:12,
  n_lags = 6
)




# p_linear_x <- ggplot(policy_lp_linear, aes(x = h, y = beta_x)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_ribbon(aes(ymin = x_lo, ymax = x_hi), alpha = 0.25) +
#   geom_line(linewidth = 0.9) +
#   geom_point(size = 1.8) +
#   labs(
#     title = "Linear policy reaction to the output gap",
#     subtitle = "Non-threshold LP; dependent variable: future change in Selic",
#     x = "Horizon",
#     y = "Marginal effect of output gap on Selic change"
#   ) +
#   theme_minimal()

p_linear_x <- ggplot(policy_lp_linear, aes(x = h, y = beta_x)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_ribbon(aes(ymin = x_lo, ymax = x_hi), alpha = 0.20) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  scale_x_continuous(breaks = 0:12) +
  labs(
    title = "Policy reaction to the output gap",
    subtitle = "Linear local projection; cumulative Selic response",
    x = "Horizon (months)",
    y = "Cumulative Selic response, p.p"
  ) +
  theme_minimal(base_size = 12)

ggsave("figures/reaction/linear_policy_reaction_output.png", p_linear_x, width = 8, height = 5, dpi = 300)


# p_linear_pi <- ggplot(policy_lp_linear, aes(x = h, y = beta_pi)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_ribbon(aes(ymin = pi_lo, ymax = pi_hi), alpha = 0.25) +
#   geom_line(linewidth = 0.9) +
#   geom_point(size = 1.8) +
#   labs(
#     title = "Linear policy reaction to the inflation gap",
#     subtitle = "Non-threshold LP; dependent variable: future change in Selic",
#     x = "Horizon",
#     y = "Marginal effect of inflation gap on Selic change"
#   ) +
#   theme_minimal()

p_linear_pi <- ggplot(policy_lp_linear, aes(x = h, y = beta_pi)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_ribbon(aes(ymin = pi_lo, ymax = pi_hi), alpha = 0.20) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  scale_x_continuous(breaks = 0:12) +
  labs(
    title = "Policy reaction to the inflation gap",
    subtitle = "Linear local projection; cumulative Selic response",
    x = "Horizon (months)",
    y = "Cumulative Selic response, p.p"
  ) +
  theme_minimal(base_size = 12)

ggsave("figures/reaction/linear_policy_reaction_inflation.png", p_linear_pi, width = 8, height = 5, dpi = 300)













# Quick sample-size / overfitting check for current linear LP spec

output_gap_var <- "ibc_output_gap"
inflation_gap_var <- "focus_inflation_gap"

control_vars <- c(
  "growth_credit_total_stock",
  "growth_icbr_commodities"
)

current_control_vars <- c(
  "growth_icbr_commodities"
)

horizons <- 0:12
n_lags <- 3

data_check <- df_no_covid

control_vars <- control_vars[control_vars %in% names(data_check)]
current_control_vars <- current_control_vars[current_control_vars %in% names(data_check)]

lag_vars <- unique(c(
  "selic_policy_rate",
  output_gap_var,
  inflation_gap_var,
  control_vars
))

df_check <- data_check %>%
  arrange(month) %>%
  make_lags(lag_vars, n_lags = n_lags)

lag_control_names <- unlist(
  lapply(lag_vars, function(v) paste0(v, "_lag", 1:n_lags))
)

rhs_vars <- c(
  output_gap_var,
  inflation_gap_var,
  current_control_vars,
  lag_control_names
)

sample_check <- bind_rows(lapply(horizons, function(h) {
  
  reg_data <- df_check %>%
    mutate(
      depvar = lead(selic_policy_rate, h) - lag(selic_policy_rate, 1)
    ) %>%
    select(month, depvar, all_of(rhs_vars)) %>%
    drop_na()
  
  tibble(
    h = h,
    n_obs = nrow(reg_data),
    n_regressors = length(rhs_vars),
    n_parameters = length(rhs_vars) + 1,
    df_residual = nrow(reg_data) - length(rhs_vars) - 1,
    obs_per_parameter = nrow(reg_data) / (length(rhs_vars) + 1)
  )
}))

print(sample_check)