"""Helpers to retrieve time series from the BCB SGS API."""


def fetch_sgs_series(series_id, start_date, end_date, name):
    """Fetch a single SGS series and return it in tabular form.

    Parameters
    ----------
    series_id : str or int
        SGS code identifying the target series.
    start_date : str
        Initial date (expected format: YYYY-MM-DD).
    end_date : str
        Final date (expected format: YYYY-MM-DD).
    name : str
        Human-readable label to assign to the downloaded series.

    Notes
    -----
    This is a starter placeholder. Full request handling and parsing are
    intentionally left for implementation in later project stages.
    """
    raise NotImplementedError("Starter scaffold: implement SGS download logic.")
