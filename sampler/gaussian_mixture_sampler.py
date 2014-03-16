import numpy as np
from sampler import RejectionSampler


class GaussianMixtureSampler(RejectionSampler):

    def __init__(self):
        pass

    def propose_func(self):
        """Propose a value uniformly between -4 and 4."""
        return np.random.uniform(-4, 4)

    def propose_logpdf(self, X):
        """Return the log-PDF value for X under the proposal distribution,
        which is just the uniform probability multiplied by a constant
        (so it is always greater than the target distribution).

        """
        if X < -4 or X > 4:
            return -np.inf
        return np.log(0.6)

    def _gaussian_logpdf(self, X, mean, var):
        """The log-PDF for X under a Gaussian distribution with mean `mean`
        and variance `var`.

        """
        logpdf = -0.5 * np.log(2 * np.pi * var) - ((X - mean) ** 2 / (2 * var))
        return logpdf

    def target_logpdf(self, X):
        """The log-PDF for X under a mixture of three Gaussian distributions.

        """
        logpdf1 = self._gaussian_logpdf(X, -1.5, 0.2)
        logpdf2 = self._gaussian_logpdf(X, 2.0, 0.1)
        logpdf3 = self._gaussian_logpdf(X, 0.2, 0.3)
        logpdfs = np.array([logpdf1, logpdf2, logpdf3])
        logpdf = np.log(np.exp(logpdfs).sum()) - np.log(3)
        return logpdf
