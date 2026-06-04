"""Data transformation helpers for the monthly credit panel."""

import numpy as np
import pandas as pd


REQUIRED_LONG_COLUMNS = ["date", "value", "series"]
CREDIT_STOCK_COLUMNS = [
    "credit_total_stock",
    "free_credit_stock",
    "directed_credit_stock",
]
LOG_CREDIT_COLUMNS = [
    "log_credit_total_stock",
    "log_free_credit_stock",
    "log_directed_credit_stock",
]
ICBR_COMMODITY_COLUMN = "icbr_commodities"
LOG_ICBR_COMMODITY_COLUMN = f"log_{ICBR_COMMODITY_COLUMN}"


def _require_columns(df, columns, context="DataFrame"):
    """Raise a clear error if any required columns are absent."""
    missing = [col for col in columns if col not in df.columns]
    if missing:
        missing_list = ", ".join(missing)
        raise ValueError(f"{context} is missing required column(s): {missing_list}")


def _series_frequency_map(dictionary_df):
    """Return a name-to-frequency map from the series dictionary."""
    if dictionary_df is None or dictionary_df.empty:
        return {}

    _require_columns(dictionary_df, ["name", "frequency"], "series dictionary")
    return (
        dictionary_df.assign(name=lambda x: x["name"].astype(str))
        .set_index("name")["frequency"]
        .astype(str)
        .str.lower()
        .to_dict()
    )


def _standardize_series_names(raw_df, dictionary_df):
    """Use dictionary names if the raw series column contains SGS IDs."""
    if dictionary_df is None or dictionary_df.empty:
        return raw_df

    required = {"name", "series_id"}
    if not required.issubset(dictionary_df.columns):
        return raw_df

    df = raw_df.copy()
    id_to_name = dict(
        zip(dictionary_df["series_id"].astype(str), dictionary_df["name"].astype(str))
    )
    df["series"] = df["series"].astype(str).replace(id_to_name)
    return df


def ensure_datetime(df, date_col="date"):
    """Return a copy with ``date_col`` converted to pandas datetime."""
    _require_columns(df, [date_col], "DataFrame")

    out = df.copy()
    out[date_col] = pd.to_datetime(out[date_col], errors="coerce")
    if out[date_col].isna().any():
        bad_count = int(out[date_col].isna().sum())
        raise ValueError(f"Could not parse {bad_count} value(s) in {date_col!r}.")
    return out


def monthly_panel_from_long(raw_df, dictionary_df=None):
    """Convert long-format SGS data into a monthly wide panel.

    Daily series are averaged within each month. Monthly and lower-frequency
    series keep the last observed value within the month.
    """
    _require_columns(raw_df, REQUIRED_LONG_COLUMNS, "raw SGS data")

    df = _standardize_series_names(raw_df, dictionary_df)
    df = ensure_datetime(df, "date")
    df["series"] = df["series"].astype(str)
    df["value"] = pd.to_numeric(df["value"], errors="coerce")
    df["month"] = df["date"].dt.to_period("M").dt.to_timestamp()

    frequency_map = _series_frequency_map(dictionary_df)
    pieces = []
    for series_name, group in df.sort_values(["series", "date"]).groupby("series"):
        frequency = frequency_map.get(series_name, "")
        if "daily" in frequency:
            monthly_values = group.groupby("month", as_index=False)["value"].mean()
        else:
            monthly_values = group.groupby("month", as_index=False)["value"].last()

        monthly_values["series"] = series_name
        pieces.append(monthly_values)

    if not pieces:
        return pd.DataFrame(columns=["month"])

    monthly_long = pd.concat(pieces, ignore_index=True)
    panel = monthly_long.pivot(index="month", columns="series", values="value")
    panel = panel.sort_index().reset_index()
    panel.columns.name = None
    return panel


# def add_credit_shares(df):
#     """Add total-credit shares for free and directed credit."""
#     component_cols = ["free_credit_stock", "directed_credit_stock"]
#     _require_columns(df, component_cols, "monthly panel")

#     out = df.copy()
#     if "credit_total_stock" not in out.columns:
#         print(
#             "Note: credit_total_stock not found; constructing it as "
#             "free_credit_stock + directed_credit_stock."
#         )
#         out["credit_total_stock"] = (
#             out["free_credit_stock"] + out["directed_credit_stock"]
#         )

#     out["directed_credit_share"] = (
#         out["directed_credit_stock"] / out["credit_total_stock"]
#     )
#     out["free_credit_share"] = out["free_credit_stock"] / out["credit_total_stock"]
#     out["credit_gap_check"] = (
#         out["credit_total_stock"]
#         - out["free_credit_stock"]
#         - out["directed_credit_stock"]
#     )
#     out["credit_gap_check_pct"] = (
#         out["credit_gap_check"] / out["credit_total_stock"]
#     )
#     return out
def add_credit_shares(df):
    """Add total-credit shares for free and directed credit."""
    required_cols = [
        "credit_total_stock",
        "free_credit_stock",
        "directed_credit_stock",
    ]
    _require_columns(df, required_cols, "monthly panel")

    out = df.copy()

    out["directed_credit_share"] = (
        out["directed_credit_stock"] / out["credit_total_stock"]
    )
    out["free_credit_share"] = (
        out["free_credit_stock"] / out["credit_total_stock"]
    )

    out["credit_gap_check"] = (
        out["credit_total_stock"]
        - out["free_credit_stock"]
        - out["directed_credit_stock"]
    )
    out["credit_gap_check_pct"] = (
        out["credit_gap_check"] / out["credit_total_stock"]
    )

    return out


def add_log_credit_variables(df):
    """Add log levels for the main credit stock variables."""
    _require_columns(df, CREDIT_STOCK_COLUMNS, "monthly panel")

    out = df.copy()
    log_columns = CREDIT_STOCK_COLUMNS.copy()
    if ICBR_COMMODITY_COLUMN in out.columns:
        log_columns.append(ICBR_COMMODITY_COLUMN)

    for col in log_columns:
        log_col = f"log_{col}"
        positive = out[col] > 0
        if (~positive & out[col].notna()).any():
            print(f"Note: {col} has non-positive values; {log_col} set to NaN there.")
        out[log_col] = np.where(positive, np.log(out[col]), np.nan)
    return out


def add_growth_rates(df):
    """Add monthly log-difference growth rates for the main credit stocks."""
    _require_columns(df, LOG_CREDIT_COLUMNS, "monthly panel")

    out = df.copy()
    out["growth_credit_total_stock"] = 100 * out["log_credit_total_stock"].diff()
    out["growth_free_credit_stock"] = 100 * out["log_free_credit_stock"].diff()
    out["growth_directed_credit_stock"] = (
        100 * out["log_directed_credit_stock"].diff()
    )
    if LOG_ICBR_COMMODITY_COLUMN in out.columns:
        out["growth_icbr_commodities"] = 100 * out[LOG_ICBR_COMMODITY_COLUMN].diff()
    return out


def add_policy_variables(df):
    """Add policy-rate and exchange-rate transformations when available."""
    out = df.copy()

    # ------------------------------------------------------------
    # Policy rate
    # ------------------------------------------------------------
    # Preferred policy-rate proxy: monthly annualized Selic.
    if "selic_policy_rate" not in out.columns:
        if "selic_monthly_annualized" in out.columns:
            out["selic_policy_rate"] = out["selic_monthly_annualized"]
        elif "selic_annual_daily" in out.columns:
            out["selic_policy_rate"] = out["selic_annual_daily"]
        elif "selic_target" in out.columns:
            out["selic_policy_rate"] = out["selic_target"]

    if "selic_policy_rate" in out.columns:
        out["delta_selic"] = out["selic_policy_rate"].diff()
    else:
        print("Note: no Selic policy-rate column found; skipping delta_selic.")

    # ------------------------------------------------------------
    # Exchange rate
    # ------------------------------------------------------------
    if "exchange_rate_usd_sale_avg" in out.columns:
        exchange_col = "exchange_rate_usd_sale_avg"
    elif "exchange_rate_commercial_buy_usd" in out.columns:
        exchange_col = "exchange_rate_commercial_buy_usd"
    else:
        exchange_col = None

    if exchange_col is not None:
        positive = out[exchange_col] > 0
        if (~positive & out[exchange_col].notna()).any():
            print(
                f"Note: {exchange_col} has non-positive values; "
                "exchange_rate_log_change set to NaN there."
            )

        exchange_log = pd.Series(
            np.where(positive, np.log(out[exchange_col]), np.nan),
            index=out.index,
        )
        out["exchange_rate_log_change"] = 100 * exchange_log.diff()
    else:
        print(
            "Note: exchange_rate_usd_sale_avg not found; "
            "skipping exchange_rate_log_change."
        )

    return out

def trim_to_core_credit_coverage(df):
    """Keep months where the core free and directed credit stocks are present."""
    core_credit_cols = ["free_credit_stock", "directed_credit_stock"]
    _require_columns(df, core_credit_cols, "monthly panel")

    out = df.copy()
    keep = out[core_credit_cols].notna().all(axis=1)
    dropped = int((~keep).sum())

    if dropped:
        print(
            "Note: dropped "
            f"{dropped:,} row(s) with missing core credit variables "
            "(free_credit_stock or directed_credit_stock)."
        )
    else:
        print(
            "Note: no rows dropped; core credit variables are available "
            "for every month."
        )

    return out.loc[keep].reset_index(drop=True)


def add_state_variables(df):
    """Add the high-directed-share regime indicator."""
    _require_columns(df, ["directed_credit_share"], "monthly panel")

    out = df.copy()
    median_share = out["directed_credit_share"].median(skipna=True)
    out["high_directed_share"] = np.where(
        out["directed_credit_share"].notna(),
        (out["directed_credit_share"] > median_share).astype(int),
        np.nan,
    )
    return out


def first_last_nonmissing(df, date_col="month"):
    """Return first and last non-missing dates for each non-date column."""
    _require_columns(df, [date_col], "DataFrame")
    dated = ensure_datetime(df, date_col)

    rows = []
    for col in dated.columns:
        if col == date_col:
            continue
        nonmissing = dated.loc[dated[col].notna(), date_col]
        rows.append(
            {
                "column": col,
                "first_nonmissing": nonmissing.min(),
                "last_nonmissing": nonmissing.max(),
            }
        )

    return pd.DataFrame(rows)


def missing_summary(df):
    """Return missing-value counts and shares by column."""
    return pd.DataFrame(
        {
            "column": df.columns,
            "missing_count": df.isna().sum().to_numpy(),
            "missing_pct": (100 * df.isna().mean()).to_numpy(),
        }
    )
