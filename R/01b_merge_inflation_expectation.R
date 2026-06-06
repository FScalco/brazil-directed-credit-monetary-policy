library(readr)
library(dplyr)
library(lubridate)
library(rbcb)

# 1. Read your existing monthly panel
df <- read_csv("../data/processed/brazil_credit_monthly_panel_with_mp_shocks.csv") %>%
  mutate(month = as.Date(month)) %>%
  arrange(month)


# 2. Download Focus 12-month-ahead IPCA expectations
focus_12m <- get_market_expectations(
  type = "inflation-12-months",
  indic = "IPCA",
  start_date = "2007-03-01"
)

# 3. Keep last available Focus observation in each month
focus_12m_monthly_clean <- focus_12m %>%
  mutate(
    focus_date = as.Date(Data),
    month = floor_date(focus_date, "month")
  ) %>%
  group_by(month) %>%
  slice_max(focus_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    month,
    focus_date,
    focus_ipca_12m_expectation = Mediana
  )


# 4. Merge Focus expectation into main panel
df <- df %>%
  left_join(focus_12m_monthly_clean, by = "month") %>%
  mutate(
    focus_inflation_gap = focus_ipca_12m_expectation - inflation_target
  )



# 5. Check merge
df %>%
  summarise(
    panel_start = min(month),
    panel_end = max(month),
    n_obs = n(),
    missing_focus = sum(is.na(focus_ipca_12m_expectation)),
    missing_focus_gap = sum(is.na(focus_inflation_gap))
  )


# 6. Save updated panel
write_csv(df, "../data/processed/brazil_credit_monthly_panel_with_mp_shocks.csv")


##### ----- Very nice plot for the robustness part. 

# library(tidyr)
# library(ggplot2)
# 
# df_gap_compare <- df %>%
#   select(month, inflation_gap, focus_inflation_gap) %>%
#   pivot_longer(
#     cols = c(inflation_gap, focus_inflation_gap),
#     names_to = "gap_type",
#     values_to = "gap"
#   ) %>%
#   mutate(
#     gap_type = recode(
#       gap_type,
#       inflation_gap = "Realized IPCA 12m gap",
#       focus_inflation_gap = "Focus expected IPCA 12m gap"
#     )
#   )
# 
# ggplot(df_gap_compare, aes(x = month, y = gap, linetype = gap_type)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_line() +
#   labs(
#     title = "Realized and Focus-based inflation gaps",
#     x = NULL,
#     y = "Percentage points",
#     linetype = NULL
#   ) +
#   theme_minimal()