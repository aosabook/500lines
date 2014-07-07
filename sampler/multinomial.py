import numpy as np
from scipy.special import gammaln


class MultinomialDistribution(object):

    def __init__(self, p, rso=None):
        """Initialize the multinomial random variable.

        Parameters
        ----------
        p: numpy array with shape (k,)
            The outcome probabilities
        rso: numpy RandomState object (default: None)
            The random number generator

        """

        # Check that the probabilities sum to 1 -- if they don't, then
        # something is wrong.
        if not np.isclose(np.sum(p), 1.0):
            raise ValueError("outcome probabilities do not sum to 1")

        # Store the parameters that were passed in
        self.p = p
        self.rso = rso

        # Precompute log probabilities, for use by the log-PMF.
        self.logp = np.log(self.p)

        # Get the appropriate function for generating the random
        # samples, depending on whether we're using a RandomState
        # object or not.
        if self.rso:
            self._sample_func = self.rso.multinomial
        else:
            self._sample_func = np.random.multinomial

    def sample(self, n):
        """Samples draws of `n` events from a multinomial distribution with
        outcome probabilities `self.p`.

        Parameters
        ----------
        n: integer
            The number of total events

        Returns
        -------
        numpy array with shape (k,)
            The sampled number of occurrences for each outcome

        """
        x = self._sample_func(n, self.p)
        return x

    def log_pmf(self, x):
        """Evaluates the log-probability mass function (log-PMF) of a
        multinomial with outcome probabilities `self.p` for a draw `x`.

        Parameters
        ----------
        x: numpy array with shape (k,)
            The number of occurrences of each outcome

        Returns
        -------
        The evaluated log-PMF for draw `x`

        """
        # Get the total number of events.
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

        # Put it all together.
        log_pmf = log_n_factorial - sum_log_xi_factorial + sum_log_pi_xi
        return log_pmf

    def pmf(self, x):
        """Evaluates the probability mass function (PMF) of a multinomial
        with outcome probabilities `self.p` for a draw `x`.

        Parameters
        ----------
        x: numpy array with shape (k,)
            The number of occurrences of each outcome

        Returns
        -------
        The evaluated PMF for draw `x`

        """
        pmf = np.exp(self.log_pmf(x))
        return pmf
