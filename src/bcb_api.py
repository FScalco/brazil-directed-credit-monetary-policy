"""Helpers to retrieve time series from the BCB SGS API."""

from pathlib import Path

import pandas as pd
import requests


SGS_URL_TEMPLATE = (
    "https://api.bcb.gov.br/dados/serie/bcdata.sgs.{series_id}/dados"
)
OUTPUT_COLUMNS = ["date", "value", "series"]


def _empty_series_frame():
    """Return an empty tidy SGS DataFrame with the project-standard columns."""
    return pd.DataFrame(columns=OUTPUT_COLUMNS)


def _format_sgs_date(value):
    """Convert a pandas-compatible date value to the SGS dd/mm/YYYY format."""
    parsed = pd.to_datetime(value)
    if pd.isna(parsed):
        raise ValueError(f"Could not parse date value: {value!r}")
    return parsed.strftime("%d/%m/%Y")


def fetch_sgs_series(series_id, start_date, end_date, name):
    """Fetch one SGS series from Banco Central do Brasil.

    Parameters
    ----------
    series_id : str or int
        SGS code identifying the target series.
    start_date : str, date-like, or pandas-compatible date
        Initial date. Strings should normally use YYYY-MM-DD format; other
        date/datetime-like objects accepted by pandas are also supported.
    end_date : str, date-like, or pandas-compatible date
        Final date. Strings should normally use YYYY-MM-DD format; other
        date/datetime-like objects accepted by pandas are also supported.
    name : str
        Label stored in the returned ``series`` column.

    Returns
    -------
    pandas.DataFrame
        Tidy DataFrame with columns ``date``, ``value``, and ``series``,
        sorted by date. If the API request fails, the payload is malformed, or
        the API returns no observations, an empty DataFrame with the same
        columns is returned.
    """
    try:
        params = {
            "formato": "json",
            "dataInicial": _format_sgs_date(start_date),
            "dataFinal": _format_sgs_date(end_date),
        }
    except (TypeError, ValueError):
        return _empty_series_frame()

    url = SGS_URL_TEMPLATE.format(series_id=series_id)

    try:
        response = requests.get(url, params=params, timeout=30)
        response.raise_for_status()
        payload = response.json()
    except (requests.RequestException, ValueError):
        return _empty_series_frame()

    if not payload:
        return _empty_series_frame()

    df = pd.DataFrame(payload)
    if not {"data", "valor"}.issubset(df.columns):
        return _empty_series_frame()

    df = df.loc[:, ["data", "valor"]].rename(
        columns={"data": "date", "valor": "value"}
    )
    df["date"] = pd.to_datetime(df["date"], format="%d/%m/%Y", errors="coerce")
    df["value"] = pd.to_numeric(
        df["value"].astype(str).str.replace(",", ".", regex=False),
        errors="coerce",
    )
    df["series"] = name

    df = df.loc[df["date"].notna(), OUTPUT_COLUMNS].sort_values("date")
    return df.reset_index(drop=True)


def save_series_csv(df, output_path):
    """Save an SGS DataFrame to CSV, creating parent directories as needed.

    Parameters
    ----------
    df : pandas.DataFrame
        DataFrame to save.
    output_path : str or pathlib.Path
        Destination CSV path.
    """
    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(output_path, index=False)
