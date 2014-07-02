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
   factory will affect the water supply of nearby residents.

3. You have a robot which captures noisy images from its camera, and
   want to determine what those images are actually showing.

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
weather conditions, which would allow you to see under which
conditions the airplane is most likely to fail.

Or, consider the object recognition example. As a simple case of the
general object recognition problem, we could suppose that our robot
needs to determine the orientation of an object (such that it can then
pick up the object and place it upright). If the robot is dealing with
three-dimensional objects, then this is an extremely challenging
problem. As we will see, even in two dimensions, identifying the
rotation of an image is not all that straightforward.

This chapter will provide an introduction to programming for sampling
methods, using the example of recovering rotation from an image as a
case study.

<!-- p(image, rotation | observed) = p(observed | rotated_image) p(rotated_image | rotation, image) p(rotation) p(image) -->
<!-- p(rotation | observed) = p(observed | rotated_image) p(rotated_image | rotation) p(rotation) -->


```
def sample_rotation():
    radians = np.random.uniform(0, 2 * np.pi)
    return radians
```

```
def sample_image():
    image = np.random.randint(0, 2, (10, 10))
    return image
```

```
def sample_rotated_image(image):
    radians = sample_rotation()
    rotated_image = rotate(image, radians)
    return rotated_image
```

```
def compare_images(image1, image2):
    pixel_distance = (image1 - image2) ** 2
    total_distance = np.sqrt(pixel_distance.sum())
    return total_distance
```


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

TODO: Give more of an intuition for how this works.

There are many other sampling methods that you could choose from, but
all of them have relatively similar design patterns behind the
implementation, so we will just be focusing on rejection sampling
here.

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

### Working in "log-space"

Why the log-PDF? When working with sampling methods, it is almost
always a good idea to work in "log-space", meaning that your functions
should always return log probabilities rather than probabilities. This
is because probabilities can get very small very quickly, resulting in
underflow errors.

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
First, "log-space" ranges from $-\infty$ to zero; in practice, this
means it ranges from the `min` value returned by `finfo` to zero. Yet,
the log of the smallest positive value is much larger than that! So,
we have greatly expaned our range of representable numbers:

```
>>> np.finfo(float).min
-1.7976931348623157e+308
>>> np.log(tiny)
-708.39641853226408
```

Second, we can perform multiplication in log-space using addition,
 because of the identity that $\log(x\cdot{}y) = \log(x) +
 \log(y)$. Thus, if we do the multiplication above in log-space, we do
 not have to deal with loss of precision due to underflow:

```
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
