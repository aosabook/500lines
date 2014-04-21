# Sampling Methods

The idea behind sampling is that we want to draw samples from a
probability distribution, but we only have the *equation* of the
distribution's Probability Density Function (PDF), rather than an
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

> *Why the log-PDF?* When working with sampling methods, it is almost
> always a good idea to work in "log-space", meaning that your
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
mixture of Gaussians. A Gaussian distribution is given by the
following equation:

$$
\mathcal(x; \mu, \sigma^2) = \frac{1}{\sqrt{2\pi\sigma^2}}\exp{\frac{-(x-\mu)^2}{2\sigma^2}}
$$

where $\mu$ and $\sigma^2$ are the parameters of the Gaussian
distribution for mean and variance, respectively. A mixture of
Gaussians is a weighted average of several Gaussians. In our case, we
are using $p(x)=\frac{1}{3}(\mathcal{N}(x; -2.5, 0.2) + \mathcal{N}(x;
2.0, 0.1) + \mathcal{N}(x; 0.2, 0.3))$.

> Note: in practice, sampling from a Gaussian distribution is fairly
> easy, and most statistics packages come with methods for doing
> so. We are using a mixture of Gaussians here mostly just for
> illustration purposes.

The
[IPython notebook](http://nbviewer.ipython.org/github/jhamrick/500lines/blob/sampler/sampler/Sampling%20example.ipynb)
creates an instance of this `GaussianMixtureSampler` and plots the
samples that are drawn.

## Possible extensions

There are several ways that the basic sampler described here could be
extended. Depending on the application, some of these extensions might
be crucially important.

### Parallelization

The sampling loop could run multiple calls to `draw` in parallel. Some
samplers (not rejection sampling) depend on samples being drawn
sequentially, though, so this won't always be applicable.

### Optimization

Function calls in Python have a high overhead. However, based on the
current implementation, we require *at least* seven Python calls per
sample (more, if the proposal and target functions themselves make
Python calls). Even simple target distributions need upwards of
$n=10000$ samples to attain a decent approximation. Thus, this
implementation will become too inefficient for even moderately complex
problems.

One solution is to rewrite the code for `draw` (and possibly even
`sample`) using an extension language like Cython or numba, or even a
lower level language like C or FORTRAN.
