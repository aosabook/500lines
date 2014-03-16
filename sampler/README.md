# Sampling Methods

The idea behind sampling is that we want to draw samples from a
probability distribution, but we only have the *equation* of the
distributions Probability Density Function (PDF), rather than an
algorithm for actually drawing samples.

## Rejection sampling

*Rejection sampling* is one of the simplest methods for doing
this. Rather than directly drawing samples from the *target*
distribution ($p$), which is the one we ultimately do want samples
from, we specify a *proposal* distribution ($q$) that we know how to
sample from. Then, the procedure is as follows:

1. Draw a sample from the proposal distribution, $x\sim q$
2. Choose a point $y$ uniformly at random in the interval $[0, q(x)]$
3. If $y < p(x)$, then accept $x$ as a sample. Otherwise, reject $x$
   and start over from step 1.

There are many other sampling methods that you could choose from, but
all of them have relatively similar design patterns behind the
implementation, so we will just be focusing on rejection sampling
here.

## Generic rejection sampler implementation

The file `sampler.py` contains the basic code for implementing a
sampler. On initialization, it takes functions to sample from the
proposal distribution and to compute the log-PDF values for both the
proposal and target distributions. 

> **Why the log-PDF?**
>
> When working with sampling methods, it is
> almost always a good idea to work in "log-space", meaning that your
> functions should always return log probabilities rather than
> probabilities. This is because probabilities can get very small very
> quickly, resulting in underflow errors.

The `RejectionSampler` class uses two methods to draw samples:
`sample` and `draw`. The first of these, `sample`, is the main outer
sampling loop: it takes one argument `n`, which is the number of
desired samples, allocates the storage for these samples, and calls
`draw` a total of `n` times to collect these samples. The `draw`
function is the actual sampling logic, following the three steps
described in the previous section.

Note that in `draw`, we need to exponentiate $\log{q(x)}$ in order to
sample $y~\mathrm{Uniform}(0, q(x))$. To make sure we don't run into
underflow errors, we first check to make sure $\log{q(x)}$ is not too
small (as defined by the minimum value that your computer can
exponentiate). If it is, then we won't be able to sample $y$, so we
`continue` to try a different value of $x$.

We also include a `plot` method, which will let us visualize the
proposal distribution, target distrubtion, and the histogram of
samples. These types of methods are important as an easy way to glance
at the samples and make sure they actually match the target
distribution.

## Example application: sampling from a mixture of Gaussians

The file `gaussian_mixture_sampler.py` creates a subclass of
`RejectionSampler` for the specific application of sampling from a
mixture of Gaussians. The IPython notebook `Sampling example.ipynb`
creates an instance of this `GaussianMixtureSampler` and plots the
samples that are drawn.
