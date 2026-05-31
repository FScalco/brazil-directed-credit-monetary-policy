"""Data transformation helpers for the monthly credit panel."""


def to_monthly():
    """Convert input data to a consistent monthly frequency.

    This starter function is a placeholder for future resampling and alignment
    logic across SGS series.
    """
    raise NotImplementedError("Starter scaffold: implement monthly transformation.")


def construct_directed_share():
    """Construct the directed-credit share in total credit.

    This starter function will later combine free and directed credit series to
    compute the analysis state variable.
    """
    raise NotImplementedError("Starter scaffold: implement directed-share construction.")


def make_high_directed_share_dummy():
    """Create a high directed-credit-share dummy indicator.

    This starter function is reserved for regime classification used in
    state-dependent local projections.
    """
    raise NotImplementedError("Starter scaffold: implement high-share dummy logic.")
