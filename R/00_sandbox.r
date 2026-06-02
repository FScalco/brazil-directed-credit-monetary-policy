# R/00_sandbox_explore_data.R

library(tidyverse)
library(lubridate)

panel_file <- "../data/processed/brazil_credit_monthly_panel.csv"

df <- readr::read_csv(panel_file) |>
  mutate(month = as.Date(month))

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
  "delta_selic",
  "ipca",
  "industrial_output_general",
  "exchange_rate_log_change",
  "free_credit_stock",
  "directed_credit_stock",
  "directed_credit_share",
  "growth_free_credit_stock",
  "growth_directed_credit_stock",
  "interest_rate_free_new_operations_total",
  "interest_rate_directed_new_operations_total"
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
  pivot_longer(-month, names_to = "variable", values_to = "value") |>
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

# Correlations -----------------------------------------------------------

df |>
  select(all_of(main_vars)) |>
  cor(use = "pairwise.complete.obs") |>
  round(2)


##########################################################################
##########################################################################
################ Contemporaneous OLS regerssions #########################
##########################################################################
##########################################################################


##########################################################################
################ Delta Selic regressions

# Very simple probing regressions ----------------------------------------

m1 <- lm(
  interest_rate_free_new_operations_total ~ delta_selic,
  data = df
)

m2 <- lm(
  interest_rate_directed_new_operations_total ~ delta_selic,
  data = df
)

m3 <- lm(
  growth_free_credit_stock ~ delta_selic,
  data = df
)

m4 <- lm(
  growth_directed_credit_stock ~ delta_selic,
  data = df
)

summary(m1)
summary(m2)
summary(m3)
summary(m4)


# There is a negative correlation between delta_selic and free credit growth, 
# but no correlation with directed credit growth


# Add controls -----------------------------------------------------------

m5 <- lm(
  interest_rate_free_new_operations_total ~
    delta_selic + ipca + industrial_output_general + exchange_rate_log_change,
  data = df
)

m6 <- lm(
  interest_rate_directed_new_operations_total ~
    delta_selic + ipca + industrial_output_general + exchange_rate_log_change,
  data = df
)

summary(m5)
summary(m6)

# Compare directed-share regimes -----------------------------------------

df |>
  group_by(high_directed_share) |>
  summarise(
    mean_directed_share = mean(directed_credit_share, na.rm = TRUE),
    mean_free_growth = mean(growth_free_credit_stock, na.rm = TRUE),
    mean_directed_growth = mean(growth_directed_credit_stock, na.rm = TRUE),
    mean_free_rate = mean(interest_rate_free_new_operations_total, na.rm = TRUE),
    mean_directed_rate = mean(interest_rate_directed_new_operations_total, na.rm = TRUE),
    .groups = "drop"
  )

# Interaction probes -----------------------------------------------------

m7 <- lm(
  interest_rate_free_new_operations_total ~
    delta_selic * high_directed_share +
    ipca + industrial_output_general + exchange_rate_log_change,
  data = df
)

m8 <- lm(
  interest_rate_directed_new_operations_total ~
    delta_selic * high_directed_share +
    ipca + industrial_output_general + exchange_rate_log_change,
  data = df
)

summary(m7)
summary(m8)

# Results: 
# These are very nice. Higher directed-credit-share periods have a higher
# free credit lending rate, and also a very negative relation between delta
# selic and free-market lending rate


##########################################################################
################ Selic level regressions

# Very simple probing regressions ----------------------------------------

m1_level <- lm(
  interest_rate_free_new_operations_total ~ selic_policy_rate,
  data = df
)

m2_level <- lm(
  interest_rate_directed_new_operations_total ~ selic_policy_rate,
  data = df
)

m3_level <- lm(
  growth_free_credit_stock ~ selic_policy_rate,
  data = df
)

m4_level <- lm(
  growth_directed_credit_stock ~ selic_policy_rate,
  data = df
)

summary(m1_level)
summary(m2_level)
summary(m3_level)
summary(m4_level) 


# Results:
# - free lending rates about 3.6 times more sensitive to SELIC level than
# directed lending rates
# - Selic level is negatively associated with free credit growth
# - Selic level has no relationship with directed credit growth


# Add controls -----------------------------------------------------------

m5_level <- lm(
  interest_rate_free_new_operations_total ~
    selic_policy_rate + ipca + industrial_output_general + exchange_rate_log_change,
  data = df
)

m6_level <- lm(
  interest_rate_directed_new_operations_total ~
    selic_policy_rate + ipca + industrial_output_general + exchange_rate_log_change,
  data = df
)

summary(m5_level)
summary(m6_level)


# This basically just shows the same thing as the bivariate model, which
# shows robustness

# Compare directed-share regimes -----------------------------------------

df |>
  group_by(high_directed_share) |>
  summarise(
    mean_directed_share = mean(directed_credit_share, na.rm = TRUE),
    mean_free_growth = mean(growth_free_credit_stock, na.rm = TRUE),
    mean_directed_growth = mean(growth_directed_credit_stock, na.rm = TRUE),
    mean_free_rate = mean(interest_rate_free_new_operations_total, na.rm = TRUE),
    mean_directed_rate = mean(interest_rate_directed_new_operations_total, na.rm = TRUE),
    mean_selic_policy_rate = mean(selic_policy_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  print(width = Inf)

# Interaction probes -----------------------------------------------------

m7_level <- lm(
  interest_rate_free_new_operations_total ~
    selic_policy_rate * high_directed_share +
    ipca + industrial_output_general + exchange_rate_log_change,
  data = df
)

m8_level <- lm(
  interest_rate_directed_new_operations_total ~
    selic_policy_rate * high_directed_share +
    ipca + industrial_output_general + exchange_rate_log_change,
  data = df
)

summary(m7_level)
summary(m8_level)





##########################################################################
##########################################################################
################ Some random LP things ###################################
##########################################################################
##########################################################################


library(dplyr)
library(purrr)
library(broom)

df_lp <- df |>
  arrange(month)

horizons <- 0:12




##########################################################################
################### Financial Passthrough




# Simple LP setup, free rate

lp_results_free_rate <- map_dfr(horizons, function(h) {
  model <- lm(
    lead(interest_rate_free_new_operations_total, h) ~
      delta_selic +
      ipca + industrial_output_general + exchange_rate_log_change,
    data = df_lp
  )
  
  tidy(model) |>
    filter(term == "delta_selic") |>
    mutate(horizon = h)
})

lp_results_free_rate




# Simple LP setup, directed rate

lp_results_directed_rate <- map_dfr(horizons, function(h) {
  model <- lm(
    lead(interest_rate_directed_new_operations_total, h) ~
      delta_selic +
      ipca + industrial_output_general + exchange_rate_log_change,
    data = df_lp
  )
  
  tidy(model) |>
    filter(term == "delta_selic") |>
    mutate(horizon = h)
})

lp_results_directed_rate





# Setup for a state dependent LP 

get_state_lp <- function(outcome_var, data, horizons = 0:12) {
  
  map_dfr(horizons, function(h) {
    
    formula_h <- as.formula(
      paste0(
        "lead(", outcome_var, ", ", h, ") ~ ",
        "delta_selic * high_directed_share + ",
        "ipca + industrial_output_general + exchange_rate_log_change"
      )
    )
    
    model <- lm(formula_h, data = data)
    
    coefs <- coef(model)
    vcov_mat <- vcov(model)
    
    b_low <- coefs["delta_selic"]
    se_low <- sqrt(vcov_mat["delta_selic", "delta_selic"])
    
    b_int <- coefs["delta_selic:high_directed_share"]
    
    b_high <- coefs["delta_selic"] + coefs["delta_selic:high_directed_share"]
    se_high <- sqrt(
      vcov_mat["delta_selic", "delta_selic"] +
        vcov_mat["delta_selic:high_directed_share", "delta_selic:high_directed_share"] +
        2 * vcov_mat["delta_selic", "delta_selic:high_directed_share"]
    )
    
    tibble(
      horizon = h,
      state = c("Low directed-share", "High directed-share", "Difference: high - low"),
      estimate = c(b_low, b_high, b_int),
      std.error = c(se_low, se_high, sqrt(vcov_mat["delta_selic:high_directed_share",
                                                   "delta_selic:high_directed_share"])),
      statistic = estimate / std.error,
      p.value = 2 * pt(abs(statistic), df = df.residual(model), lower.tail = FALSE),
      ci_low = estimate - 1.96 * std.error,
      ci_high = estimate + 1.96 * std.error
    )
  })
}



# Running it for both
lp_free_rate_by_state <- get_state_lp(
  outcome_var = "interest_rate_free_new_operations_total",
  data = df_lp,
  horizons = 0:12
)

lp_directed_rate_by_state <- get_state_lp(
  outcome_var = "interest_rate_directed_new_operations_total",
  data = df_lp,
  horizons = 0:12
)

lp_free_rate_by_state
lp_directed_rate_by_state



# Plotting free lending rate
p1_free_rate_state <- lp_free_rate_by_state |>
  filter(state != "Difference: high - low") |>
  ggplot(aes(x = horizon, y = estimate, ymin = ci_low, ymax = ci_high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  geom_point() +
  facet_wrap(~ state) +
  labs(
    title = "State-dependent LP: free lending-rate response to Selic changes",
    x = "Horizon",
    y = "Response to delta_selic"
  )



# Plotting directed lending rate
p2_directed_state <- lp_directed_rate_by_state |>
  filter(state != "Difference: high - low") |>
  ggplot(aes(x = horizon, y = estimate, ymin = ci_low, ymax = ci_high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  geom_point() +
  facet_wrap(~ state) +
  labs(
    title = "State-dependent LP: directed lending-rate response to Selic changes",
    x = "Horizon",
    y = "Response to delta_selic"
  )



# Plotting whether difference between states is significant for free rate
p3_free_rate_difference <- lp_free_rate_by_state |>
  filter(state == "Difference: high - low") |>
  ggplot(aes(x = horizon, y = estimate, ymin = ci_low, ymax = ci_high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  geom_point() +
  labs(
    title = "Difference in LP response: high minus low directed-share state",
    subtitle = "Outcome: free lending rate",
    x = "Horizon",
    y = "Difference in response"
  )


# Plotting whether difference between states is significant for directed rate
p4_directed_difference <- lp_directed_rate_by_state |>
  filter(state == "Difference: high - low") |>
  ggplot(aes(x = horizon, y = estimate, ymin = ci_low, ymax = ci_high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  geom_point() +
  labs(
    title = "Difference in LP response: high minus low directed-share state",
    subtitle = "Outcome: directed lending rate",
    x = "Horizon",
    y = "Difference in response"
  )


ggsave("../figures/lp_free_rate_by_state.png", p1_free_rate_state, width = 8, height = 5, dpi = 300)
ggsave("../figures/lp_directed_rate_by_state.png", p2_directed_state, width = 8, height = 5, dpi = 300)
ggsave("../figures/lp_free_rate_state_diff.png", p3_free_rate_difference, width = 9, height = 5, dpi = 300)
ggsave("lp_directed_rate_state_diff.png", p4_directed_difference, width = 9, height = 5, dpi = 300)










##########################################################################
################### Real Economy Passthrough





# Basic setup 

df_lp <- df |>
  arrange(month) |>
  mutate(
    ipca_l1 = lag(ipca, 1),
    industrial_output_l1 = lag(industrial_output_general, 1),
    exchange_rate_log_change_l1 = lag(exchange_rate_log_change, 1),
    selic_policy_rate_l1 = lag(selic_policy_rate, 1)
  )

horizons <- 0:12


# Simple LP for IPCA
lp_results_ipca <- map_dfr(horizons, function(h) {
  
  model <- lm(
    lead(ipca, h) ~
      delta_selic +
      ipca_l1 +
      industrial_output_l1 +
      exchange_rate_log_change_l1 + 
      selic_policy_rate_l1,
    data = df_lp
  )
  
  tidy(model) |>
    filter(term == "delta_selic") |>
    mutate(horizon = h)
})

lp_results_ipca



# Simple LP for Industrial Output
lp_results_industrial_output <- map_dfr(horizons, function(h) {
  
  model <- lm(
    lead(industrial_output_general, h) ~
      delta_selic +
      industrial_output_l1 +
      ipca_l1 +
      exchange_rate_log_change_l1 +
      selic_policy_rate_l1,
    data = df_lp
  )
  
  tidy(model) |>
    filter(term == "delta_selic") |>
    mutate(horizon = h)
})

lp_results_industrial_output


asdf


# ##########################################################################
# ################### Using the mp shocks
# 
# panel_file <- "../data/processed/brazil_credit_monthly_panel_with_mp_shocks.csv"
# 
# df <- readr::read_csv(panel_file) |>
#   mutate(month = as.Date(month))
# 
# df_lp <- df |>
#   arrange(month) |>
#   mutate(
#     ipca_l1 = lag(ipca, 1),
#     industrial_output_l1 = lag(industrial_output_general, 1),
#     exchange_rate_log_change_l1 = lag(exchange_rate_log_change, 1),
#     selic_policy_rate_l1 = lag(selic_policy_rate, 1)
#   )
# 
# horizons <- 0:12
# 
# # Free lending rate
# lp_results_free_rate_shock <- map_dfr(horizons, function(h) {
#   model <- lm(
#     lead(interest_rate_free_new_operations_total, h) ~
#       mp_shock_pp_zero +
#       ipca + industrial_output_general + exchange_rate_log_change,
#     data = df_lp
#   )
#   
#   tidy(model) |>
#     filter(term == "mp_shock_pp_zero") |>
#     mutate(horizon = h)
# })
# 
# lp_results_free_rate_shock
# 
# 
# # Directed lending rate
# lp_results_directed_rate_shock <- map_dfr(horizons, function(h) {
#   model <- lm(
#     lead(interest_rate_directed_new_operations_total, h) ~
#       mp_shock_pp_zero +
#       ipca + industrial_output_general + exchange_rate_log_change,
#     data = df_lp
#   )
#   
#   tidy(model) |>
#     filter(term == "mp_shock_pp_zero") |>
#     mutate(horizon = h)
# })
# 
# lp_results_directed_rate_shock
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ##########################################################################
# ################### Real economy passthrough using MP shocks
# 
# 
# # Setup
# 
# df_lp <- df |>
#   arrange(month) |>
#   mutate(
#     ipca_l1 = lag(ipca, 1),
#     industrial_output_l1 = lag(industrial_output_general, 1),
#     exchange_rate_log_change_l1 = lag(exchange_rate_log_change, 1),
#     selic_policy_rate_l1 = lag(selic_policy_rate, 1),
#     growth_credit_total_stock_l1 = lag(growth_credit_total_stock, 1),
#     growth_free_credit_stock_l1 = lag(growth_free_credit_stock, 1),
#     growth_directed_credit_stock_l1 = lag(growth_directed_credit_stock, 1)
#   )
# 
# horizons <- 0:12
# 
# run_real_lp <- function(outcome_var, data, horizons = 0:12) {
#   
#   map_dfr(horizons, function(h) {
#     
#     formula_h <- as.formula(
#       paste0(
#         "lead(", outcome_var, ", ", h, ") ~ ",
#         "mp_shock_pp_zero + ",
#         "ipca_l1 + industrial_output_l1 + ",
#         "exchange_rate_log_change_l1 + selic_policy_rate_l1"
#       )
#     )
#     
#     model <- lm(formula_h, data = data)
#     
#     tidy(model) |>
#       filter(term == "mp_shock_pp_zero") |>
#       mutate(
#         outcome = outcome_var,
#         horizon = h
#       )
#   })
# }
# 
# 
# # Results
# 
# real_outcomes <- c(
#   "ipca",
#   "industrial_output_general",
#   "growth_credit_total_stock",
#   "growth_free_credit_stock",
#   "growth_directed_credit_stock"
# )
# 
# lp_real_results <- map_dfr(
#   real_outcomes,
#   ~ run_real_lp(.x, data = df_lp, horizons = horizons)
# )
# 
# lp_real_results
# 



