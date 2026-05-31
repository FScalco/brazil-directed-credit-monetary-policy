# Project instructions for coding agents

## Project
Monetary Policy Transmission and Directed Credit in Brazil

## Research question
Do monetary policy shocks pass through differently to free credit and directed/earmarked credit in Brazil? Does transmission differ when the directed-credit share is high?

## Main empirical strategy
The project uses monthly Brazilian macro-financial data and local projections.

Baseline LP:
y_{t+h} - y_{t-1} = alpha_h + beta_h * MPShock_t + controls + error

State-dependent LP:
y_{t+h} - y_{t-1} =
alpha_h
+ beta_h * MPShock_t
+ theta_h * MPShock_t * HighDirectedShare_t
+ controls
+ error

Interpretation:
- beta_h is the effect in low-directed-credit-share periods.
- beta_h + theta_h is the effect in high-directed-credit-share periods.
- theta_h is the difference between high and low regimes.

## Repository structure
- data/raw/: raw downloaded data. Do not commit large raw data files.
- data/processed/: cleaned data. Do not commit large processed data files.
- data/series_dictionary.csv: manually maintained list of data series and SGS codes.
- notebooks/: exploratory and analysis notebooks.
- src/: reusable Python functions.
- output/figures/: generated figures. Do not commit unless explicitly requested.
- output/tables/: generated tables. Do not commit unless explicitly requested.
- R/legacy/: old R scripts preserved for reference.

## Coding style
- Use Python first.
- Keep functions small and readable.
- Prefer pandas, numpy, requests, statsmodels, and matplotlib.
- Do not over-engineer.
- Do not implement unrelated features.
- Do not delete legacy R code unless explicitly instructed.
- Add comments where research assumptions enter the code.
- All BCB SGS series IDs must be easy to edit and manually verifiable.

## Data source
Use the Banco Central do Brasil SGS API:
https://api.bcb.gov.br/dados/serie/bcdata.sgs.{series_id}/dados?formato=json&dataInicial={start_date}&dataFinal={end_date}

Dates should be parsed from Brazilian dd/mm/YYYY format.

## Immediate development order
1. Implement BCB SGS downloader.
2. Build clean monthly panel.
3. Add descriptive plots.
4. Add baseline local projections.
5. Add state-dependent local projections.