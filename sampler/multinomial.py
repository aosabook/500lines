import numpy as np
from scipy.special import gammaln


def multinomial_logpmf(x, p):
    """Evaluates the log-probability mass function (log-PMF) of a
    multinomial with event probabilities `p` for a draw `x`.

    Parameters
    ----------
    x: numpy array with shape (k,)
        The number of occurrences of each event
    p: numpy array with shape (k,)
        The event probabilities

    Returns
    -------
    The evaluated log-PMF for draw `x`

    """
    # get the total number of events
    n = np.sum(x)
    # equivalent to log(n!)
    numerator = gammaln(n + 1)
    # equivalent to log(x1! * ... * xk!)
    denominator = np.sum(gammaln(x + 1))
    # equivalent to log(p1^x1 * ... * pk^xk)
    weights = np.sum(np.log(p) * x)
    log_pmf = numerator - denominator + weights
    return log_pmf


def multinomial_pmf(x, p):
    """Evaluates the probability mass function (PMF) of a multinomial
    with event probabilities `p` for a draw `x`.

    Parameters
    ----------
    x: numpy array with shape (k,)
        The number of occurrences of each event
    p: numpy array with shape (k,)
        The event probabilities

    Returns
    -------
    The evaluated PMF for draw `x`

    """
    pmf = np.exp(multinomial_logpmf(x, p))
    return pmf


def sample_multinomial(n, p, rso=None):

    """Samples draws of `n` events from a multinomial distribution with
    event probabilities `p`.

    Parameters
    ----------
    n: integer
        The number of total events
    p: numpy array with shape (k,)
        The event probabilities
    rso: numpy RandomState object (default: None)
        The random number generator

    Returns
    -------
    numpy array with shape (k,)
        The sampled number of occurrences for each event

    """
    # get the appropriate function for generating the
    # random sample
    if rso:
        func = rso.multinomial
    else:
        func = np.random.multinomial

    x = func(n, p)
    return x
