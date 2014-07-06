import numpy as np
from scipy.special import gammaln


class MultinomialDistribution(object):

    def __init__(self, p, rso=None):
        """Initialize the multinomial random variable.

        Parameters
        ----------
        p: numpy array with shape (k,)
            The event probabilities
        rso: numpy RandomState object (default: None)
            The random number generator

        """
        self.p = p
        self.logp = np.log(self.p)
        self.rso = rso

        # Get the appropriate function for generating the random
        # samples, depending on whether we're using a RandomState
        # object or not.
        if self.rso:
            self._sample_func = self.rso.multinomial
        else:
            self._sample_func = np.random.multinomial

        # Check that the probabilities sum to 1 -- if they don't, then
        # something is wrong.
        if not np.isclose(np.sum(self.p), 1.0):
            raise ValueError("event probabilities do not sum to 1")

    def logpmf(self, x):
        """Evaluates the log-probability mass function (log-PMF) of a
        multinomial with event probabilities `self.p` for a draw `x`.

        Parameters
        ----------
        x: numpy array with shape (k,)
            The number of occurrences of each event

        Returns
        -------
        The evaluated log-PMF for draw `x`

        """
        # Get the total number of events.
        n = np.sum(x)

        # equivalent to log(n!)
        numerator = gammaln(n + 1)
        # equivalent to log(x1! * ... * xk!)
        denominator = np.sum(gammaln(x + 1))

        # If one of the values of self.p is 0, then the corresponding
        # value of self.logp will be -inf. If the corresponding value
        # of x is 0, then multiplying them together will give nan, but
        # we want it to just be 0.
        all_weights = self.logp * x
        all_weights[x == 0] = 0
        # equivalent to log(p1^x1 * ... * pk^xk)
        weights = np.sum(all_weights)

        # Put it all together.
        log_pmf = numerator - denominator + weights
        return log_pmf

    def pmf(self, x):
        """Evaluates the probability mass function (PMF) of a multinomial
        with event probabilities `self.p` for a draw `x`.

        Parameters
        ----------
        x: numpy array with shape (k,)
            The number of occurrences of each event

        Returns
        -------
        The evaluated PMF for draw `x`

        """
        pmf = np.exp(self.logpmf(x))
        return pmf

    def sample(self, n):
        """Samples draws of `n` events from a multinomial distribution with
        event probabilities `self.p`.

        Parameters
        ----------
        n: integer
            The number of total events

        Returns
        -------
        numpy array with shape (k,)
            The sampled number of occurrences for each event

        """
        x = self._sample_func(n, self.p)
        return x
