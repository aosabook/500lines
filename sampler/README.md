# Sampling Methods

Note: this chapter assumes some familiarity with statistics and
probability theory.

Frequently, in computer science and engineering, we run into problems
that can't be solved using an equation. These problems usually involve
complex systems, noisy inputs, or both. Here are just a few examples
of real-world problems that do not have exact, analytic solutions:

1. You have built a computer model of an airplane, and want to
   determine how well the airplane will hold up under different
   weather conditions.

2. You want to determine whether chemical runoff from a proposed
   factory will affect the water supply of nearby residents, based on
   a model of groundwater diffusion.

3. You have a robot which captures noisy images from its camera, and
   want to recover the three-dimensional structure of the object that
   those images depict.

4. You want to compute how likely you are to win at chess if you take
   a particular move.

Even though these types of problems cannot be solved exactly, we can
often achieve an approximate solution to them using techniques known
as *Monte Carlo sampling* methods. In Monte Carlo methods, the key
idea is to take many *samples*, which will then allow you to estimate
the solution.

Consider the airplane example above. Weather is a fairly chaotic
system, meaning that it is impossible to compute *exactly* whether the
airplane will survive a particular weather situation. Instead, you
could simulate the behavior of the airplane under many different
weather conditions, multiple times, which would allow you to see under
which conditions the airplane is most likely to fail.

This chapter will provide an introduction to programming for
probabilities, and in particular for sampling methods like the ones
you would need to compute solutions to the examples above.

## Programming with probabilities

When working with probabilities, there are a few standard
practices. Following these will make your code cleaner, easier to
test, and less buggy in general.

### Probabilities and samples

Typically when we talk about probability distributions, we will use
mathematical notation like $p(x)$ to indicate that $p$ is the
*probability density function* (PDF) or *probability mass function*
(PMF) over values $x$ of a random variable.

There are two things that we might want to do with a probability
distribution. Given a value (or location) $x$, we might want to
*evaluate* what the probability density (or mass) is at that
location. In mathematical notation, we would write this as $p(x)$ (the
probability density at the value $x$).

Given the PDF or PMF, we might also want to *sample* a value $x$ in a
manner proportional to the distribution (such that we are more likely
to get a sample at places where the probability is higher). In
mathematical notation, we would write this as $x\sim p$ or $x\sim
p(x)$, to indicate that $x$ is sampled proportional to $p$.

Evaluation and sampling are both critical pieces to working with
probabilities, so when you write code to work with probabilities, you
need to include functionality for both of these pieces.

As a simple example, let's consider the *Gaussian distribution*. It
has the following equation:

$$
\mathcal{N}(x; \mu, \sigma^2) = \frac{1}{\sqrt{2\pi\sigma^2}}\exp{\frac{-(x-\mu)^2}{2\sigma^2}}
$$

The Gaussian, also known as the "normal distribution" or "bell curve",
is one of the most commonly used distributions. They are used to model
many types of random variables, such as TODO: list some examples

#### Evaluating the Gaussian PDF

TODO

```
def gaussian_pdf(x, mean, variance):
    """Evaluates the PDF of a Gaussian with mean `mean` and
    variance `variance` at location(s) `x`.

    Parameters
    ----------
    x: numpy array
        The locations to be evaluated
    mean: float
        The mean parameter of the Gaussian
    variance: float
        The variance parameter of the Gaussian

    Returns
    -------
    numpy array with the same shape as x
        The evaluated PDF at locations x

    """
    constant = 1.0 / np.sqrt(2 * np.pi * variance)
    exp = np.exp(-(x - mean) ** 2 / (2 * variance))
    p = constant * exp
    return p
```

#### Sampling from the Gaussian PDF

TODO

```
def gaussian_sample(n, mean, variance):
    """Samples `n` values proportional to a Gaussian PDF
    with mean `mean` and variance `variance`.

    Parameters
    ----------
    n: integer or tuple of integers
        The number of samples to take
    mean: float
        The mean parameter of the Gaussian
    variance: float
        The variance parameter of the Gaussian

    Returns
    -------
    numpy array with shape `n`
        The sampled values

    """
    standard_deviation = np.sqrt(variance)
    x = np.random.norm(mean, standard_deviation, n)
    return x
```

### Working in "log-space"

When working with sampling methods, it is almost always a good idea to
work in "log-space", meaning that your functions should always return
log probabilities rather than probabilities. This is because
probabilities can get very small very quickly, resulting in underflow
errors.

To motivate this, consider that probabilities must range between 0 and
1 (inclusive). NumPy has a useful function, `finfo`, that will tell us
the limits of floating point values for our system. For example, on a
64-bit machine, we see that the smallest usable positive number (given
by `tiny`) is:

```
>>> import numpy as np
>>> np.finfo(float).tiny
2.2250738585072014e-308
```

While that may seem very small, it is not unusual to encounter
probabilities of this magnitude, or even smaller! Moreover, it is a
common operation to multiply probabilities, yet if we try to do this
with very small probabilities, we encounter underflow problems:

```
>>> tiny = np.finfo(float).tiny
>>> tiny * tiny
0.0
```

However, taking the log can help alleviate this issue for two reasons.

#### Range of possible values

First, "log-space" ranges from $-\infty$ to zero. Since $-\infty$ is
not an actual number, this means in practice that it ranges from the
`min` value returned by `finfo` (which is the smallest number that can
be represented) to zero:

```
>>> np.finfo(float).min
-1.7976931348623157e+308
```

But, the log of the smallest positive value is much larger than the
`min` value:

```
>>> np.log(tiny)
-708.39641853226408
```

So, by working in log-space, we can greatly expand our range of
representable numbers.

#### Floating-point error

Second, we can perform multiplication in log-space using addition,
 because of the identity that $\log(x\cdot{}y) = \log(x) +
 \log(y)$. Thus, if we do the multiplication above in log-space, we do
 not have to worry (as much) about loss of precision due to underflow:

```
>>> np.log(tiny * tiny)
-inf
>>> np.log(tiny) + np.log(tiny)
-1416.7928370645282
```

Of course, this solution is not a magic bullet. If we need to bring
the number *out* of log-space (for example, to add probabilities,
rather than multiply them), then we are back to the issue of
underflow:

```
>>> tiny*tiny
0.0
>>> np.exp(np.log(tiny) + np.log(tiny))
0.0

```

#### Revisiting the Gaussian PDF

Now with this knowledge of the issues that can come up if we do not
work in log-space, we can rewrite our Gaussian PDF to be a *log*-PDF
instead:

```
def gaussian_logpdf(x, mean, variance):
    """Evaluates the log-PDF of a Gaussian with mean `mean` and
    variance `variance` at location(s) `x`.

    Parameters
    ----------
    x: numpy array
        The locations to be evaluated
    mean: float
        The mean parameter of the Gaussian
    variance: float
        The variance parameter of the Gaussian

    Returns
    -------
    numpy array with the same shape as x
        The evaluated log-PDF at locations x

    """
    log_constant = -0.5 * np.log(2 * np.pi * variance)
    log_exp = -((x - mean) ** 2 / (2 * variance))
    logp = log_constant + log_exp
    return logp
```

If we really do need the PDF, and not the log-PDF, we can still
compute it. However, it is generally better to *first* compute it in
log-space, and then exponentiate it if we need to take it out of
log-space:

```
def gaussian_pdf(x, mean, variance):
    """Evaluates the PDF of a Gaussian with mean `mean` and
    variance `variance` at location(s) `x`.

    Parameters
    ----------
    x: numpy array
        The locations to be evaluated
    mean: float
        The mean parameter of the Gaussian
    variance: float
        The variance parameter of the Gaussian

    Returns
    -------
    numpy array with the same shape as x
        The evaluated PDF at locations x

    """
    p = np.exp(gaussian_logpdf(x, mean, variance))
    return p
```

### Seeding the random number generator

Even though we are working with probabilities, we sometimes *don't*
want our results to be random. For example, TODO: give an example

In order to allow for the generation of "deterministically random"
numbers, we need to tell our sampling functions *how* to generate the
random numbers. We can accomplish this through use of a NumPy
`RandomState` object, which is essentially a random number generator
object that can be passed around. We can create it like this:

```
rso = np.random.RandomState(230489)
```

where the number passed to the `RandomState` constructor is the *seed*
for the random number generator. As long as we instantiate it with the
same seed, a `RandomState` object will produce the same "random"
numbers in the same order, thus ensuring replicability.

So, we can modify our sampling function to optionally take in a
`RandomState` object. If such an object is passed in, then the
function will use that object to sample the values; otherwise, it will
use the usual function from `np.random`:

```
def gaussian_sample(n, mean, variance, rso=None):
    """Samples `n` values proportional to a Gaussian PDF
    with mean `mean` and variance `variance`.

    Parameters
    ----------
    n: integer or tuple of integers
        The number of samples to take
    mean: float
        The mean parameter of the Gaussian
    variance: float
        The variance parameter of the Gaussian
    rso: numpy RandomState object (default: None)
        The random number generator

    Returns
    -------
    numpy array with shape `n`
        The sampled values

    """
    # get the appropriate function for generating the
    # random sample
    if rso:
        func = rso.norm
    else:
        func = np.random.norm

    standard_deviation = np.sqrt(variance)
    x = func(mean, standard_deviation, n)
    return x
```

## Rejection sampling

More formally, the idea behind sampling is that we want to draw
samples from a probability distribution, but we can only evaluate the
probability density (or mass) at a particular point -- we do not
actually have a method of drawing samples in proportion to that
distribution.

There are many different types of Monte Carlo sampling methods,
but we will focus on only one here: rejection sampling.

### Mathematical background

*Rejection sampling* is one of the simplest methods for drawing
approximate samples from a distribution. Rather than directly drawing
samples from the *target* distribution ($p$), which is the one we
ultimately do want samples from, we specify a *proposal* distribution
($q$) that we know how to sample from. The only constraint on $q$ is
that $q(x)>=p(x)$ for all $x$ (and it is not necessary that $q(x)$ be
a proper distribution, i.e. it is ok if it does not integrate to
1). Then, the procedure is as follows:

1. Draw a sample from the proposal distribution, $x\sim q$
2. Choose a point $y$ uniformly at random in the interval $[0, q(x)]$
3. If $y < p(x)$, then accept $x$ as a sample. Otherwise, reject $x$
   and start over from step 1.

By repeating this procedure many times, we can get an estimate of what
the true probability distribution $p(x)$ look like.

### Example: measuring the depth of the sea floor

To give a more intuitive example of what all this math means, consider
the problem of trying to map the depth of the ocean floor. With
technology like sonar, this isn't so difficult, so let's instead
imagine that we're 15th century sailors, and the best we have is an
anchor on the end of a very long rope. For purposes of illustration,
let's pretend we can't measure the length of the rope, either -- we
can only know whether it has hit the bottom of the ocean.

Then, the three steps listed above can be reinterpreted as follows:

1. Pick a random point $x$ on the surface of the ocean.
2. Drop the anchor into the ocean, and let it go down for a random
   length between the top of the ocean, and the end of the rope.
3. If the anchor hits the sea floor, then accept $x$ as a
   sample. Otherwise, reject $x$ and start over from step 1.

By keeping track of the locations in which the anchor touched the
bottom, we can get a good estimate of where the ocean is very deep
(few samples), and where it is relatively shallow (many samples).

It makes sense why this works: if the ocean is deep at $x$, then most
of the time, the anchor won't touch the bottom (it will only if we
chose a very long length of rope). Thus, we will end up with very few
samples in places where the ocean is deep. Conversely, if the ocean is
very shallow at $x$, then most of the time the anchor *will* touch the
bottom (it won't only if we choose a very short length of rope). So,
we will end up with a lot of samples in places where the ocean is
shallow.

TODO: it would be nice to have an illustration of this

### Why rejection sampling?

If it seems like rejection sampling is an awfully inefficient method
to estimate anything, you're right: it is! However, it is a useful
method to learn *first*, because -- despite being so simple -- it
still follows design patterns that are common to other sampling
methods.

The other sampling methods which exist are much more powerful, but
also much more complicated. While we unfortunately don't have time to
cover them in this chapter, interested readers are encouraged to look
further into slice sampling, the Metropolis-Hastings algorithm, and
Gibbs sampling, all of which are popular choices of Monte Carlo
sampling algorithms. TODO: references

## Generic rejection sampler implementation

The file `sampler.py` contains the basic code for implementing a
sampler. On initialization, it takes functions to sample from the
proposal distribution and to compute the log-PDF values for both the
proposal and target distributions.

TODO: discuss the design decision of having `RejectionSampler` take
the functions, and then requiring users to instantiate it directly,
rather than subclassing it and having users write write the functions
as methods of the subclass.

TODO: include discussion about using `import numpy as np` rather than
`import numpy`.

TODO: include discussion about variable names (descriptive vs math?)

### Structure of the rejection sampler

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

The
[IPython notebook](http://nbviewer.org/github/jhamrick/500lines/blob/sampler/sampler/Sampling%20example.ipynb)
uses `RejectionSampler` for the specific application of sampling from
a mixture of Gaussians. A Gaussian distribution is given by the
following equation:

$$
\mathcal{N}(x; \mu, \sigma^2) = \frac{1}{\sqrt{2\pi\sigma^2}}\exp{\frac{-(x-\mu)^2}{2\sigma^2}}
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

TODO: include multi-dimensional example

TODO: include discussion about where to include visualization code

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
