import numpy as np
from scipy.special import gammaln


class MultinomialDistribution(object):

    def __init__(self, p, rso=np.random):
        """Initialize the multinomial random variable.

        Parameters
        ----------
        p: numpy array of length `k`
            The outcome probabilities
        rso: numpy RandomState object (default: np.random)
            The random number generator

        """

        # Check that the probabilities sum to 1. If they don't, then
        # something is wrong! We use `np.isclose` rather than checking
        # for exact equality because in many cases, we won't have
        # exact equality due to floating-point error.
        if not np.isclose(np.sum(p), 1.0):
            raise ValueError("outcome probabilities do not sum to 1")

        # Store the parameters that were passed in
        self.p = p
        self.rso = rso

        # Precompute log probabilities, for use by the log-PMF, for
        # each element of `self.p` (the function `np.log` operates
        # elementwise over NumPy arrays, as well as on scalars.)
        self.logp = np.log(self.p)

    def sample(self, n):
        """Samples draws of `n` events from a multinomial distribution with
        outcome probabilities `self.p`.

        Parameters
        ----------
        n: integer
            The number of total events

        Returns
        -------
        numpy array of length `k`
            The sampled number of occurrences for each outcome

        """
        x = self.rso.multinomial(n, self.p)
        return x

    def log_pmf(self, x):
        """Evaluates the log-probability mass function (log-PMF) of a
        multinomial with outcome probabilities `self.p` for a draw `x`.

        Parameters
        ----------
        x: numpy array of length `k`
            The number of occurrences of each outcome

        Returns
        -------
        The evaluated log-PMF for draw `x`

        """
        # Get the total number of events
        n = np.sum(x)

        # equivalent to log(n!)
        log_n_factorial = gammaln(n + 1)
        # equivalent to log(x1! * ... * xk!)
        sum_log_xi_factorial = np.sum(gammaln(x + 1))

        # If one of the values of self.p is 0, then the corresponding
        # value of self.logp will be -inf. If the corresponding value
        # of x is 0, then multiplying them together will give nan, but
        # we want it to just be 0.
        log_pi_xi = self.logp * x
        log_pi_xi[x == 0] = 0
        # equivalent to log(p1^x1 * ... * pk^xk)
        sum_log_pi_xi = np.sum(log_pi_xi)

        # Put it all together
        log_pmf = log_n_factorial - sum_log_xi_factorial + sum_log_pi_xi
        return log_pmf

    def pmf(self, x):
        """Evaluates the probability mass function (PMF) of a multinomial
        with outcome probabilities `self.p` for a draw `x`.

        Parameters
        ----------
        x: numpy array of length `k`
            The number of occurrences of each outcome

        Returns
        -------
        The evaluated PMF for draw `x`

        """
        pmf = np.exp(self.log_pmf(x))
        return pmf
