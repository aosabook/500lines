# Sampling Methods

Note: this chapter assumes some familiarity with statistics and
probability theory.

## Introduction

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

### What, exactly, is sampling?

The term *sampling* means generating random values from some
probability distribution. For example, the value you get from rolling
a six-sided die is a sample. The card you draw from the top of the
deck after it has been shuffled is a sample. The location where the
dart hits the board is also a sample. The only difference between
these various samples is that they are generated from different
*probability distributions*. In the case of the die, the distribution
places equal weight across six values. In the case of the card, the
distribution places equal weight across 52 values. In the case of the
dart board, the distribution places weight across a circular area
(though it might not be uniformly distributed, depending on your skill
as a dart player).

There are two ways we usually want to use samples. The first is just
to generate a random value to be used later: for example, randomly
drawing cards in a computer game of poker. The second way that samples
are used is for estimation. For example, if you suspected that your
friend was playing with loaded dice, you might want to roll the dice
many times to see if some numbers came up more often than you would
expect. Or, you might just want to characterize the range of
possibilities, as in the airplane example above. Weather is a fairly
chaotic system, meaning that it is impossible to compute *exactly*
whether the airplane will survive a particular weather
situation. Instead, you could simulate the behavior of the airplane
under many different weather conditions, multiple times, which would
allow you to see under which conditions the airplane is most likely to
fail.

### Programming with samples and probabilities

As with most applications in computer science, you can make design
decisions when programming with samples and probabilities that will
influence the overall cleanliness, coherence, and correctness of your
code. In this chapter, we will go through a simple example of how to
sample random items in a computer game, building up to a more generic
implementation of a commonly used type of sampler called a *rejection
sampler*. In particular, we will focus on the design decisions which
are specific to working with probabilities: including functions both
for sampling and for evaluating probabilities, working in "log-space",
allowing reproducibility, and separating the process of generating
samples from the specific application.

> A brief aside about notation
>
> Typically when we talk about probability distributions, we will use
> mathematical notation like $p(x)$ to indicate that $p$ is the
> *probability density function* (PDF) or *probability mass function*
> (PMF) over values $x$ of a random variable.
> 
> There are two things that we might want to do with a probability
> distribution. Given a value (or location) $x$, we might want to
> *evaluate* what the probability density (or mass) is at that
> location. In mathematical notation, we would write this as $p(x)$
> (the probability density at the value $x$).
>
> Given the PDF or PMF, we might also want to *sample* a value $x$ in
> a manner proportional to the distribution (such that we are more
> likely to get a sample at places where the probability is
> higher). In mathematical notation, we would write this as $x\sim p$,
> to indicate that $x$ is sampled proportional to $p$.

## Sampling magical items

As a simple example to demonstrate the various design decisions
involved with programming with probabilities, let's imagine we're
writing a roleplaying game (RPG). We would like a method of generating
bonus stats for the magical items that are randomly dropped by
monsters. We might decide that the maximum bonus we want an item to
confer is +5, and that higher bonuses are less likely than lower
bonuses. If $B$ is a random variable over the values of the bonus,
then:

$$
P(B=\mathrm{+1}) = 0.55\\
P(B=\mathrm{+2}) = 0.25\\
P(B=\mathrm{+3}) = 0.12\\
P(B=\mathrm{+4}) = 0.06\\
P(B=\mathrm{+5}) = 0.02
$$

We can also specify that there are six stats (dexterity, constitution,
strength, intelligence, wisdom, and charisma) that our bonus should be
distributed between. So, an item with a +5 bonus could have those
points distributed across different stats (e.g., +2 wisdom and +3
intelligence) or concentrated within a single stat (e.g., +5
charisma).

How would we randomly sample these stats? The easiest way is probably
to first sample the overall item bonus. Then, we sample the way the
bonus is distributed across the stats. Conveniently, the probability
distributions of the bonus and the way that it is distributed are both
instances of the *multinomial distribution*.

## The multinomial distribution

The multinomial distribution is used when you have several categories
of events, and you want to characterize the probability of some
combination of events happening.  The classic example used to describe
the multinomial distribution is the *ball and urn* example. The idea
is that you have an urn with different colored balls in it (for
example, 30% red, 20% blue, and 50% green). You pull out a ball,
record its color, put it back in the urn, and then repeat this
multiple times. The multinomial distribution then tells you what the
probability is of selecting the balls that you did.

Note: the code in this section is also located in the file
`multinomial.py`.

### Sampling from a multinomial distribution

Taking a sample from a multinomial is actually fairly straightforward,
because NumPy provides us a function to do so already: the `np.random`
module includes functions to sample from many different types of
distributions.

Before we can take a sample, we need to specify two *parameters*: the
number of events, $n$ (in the urn example, corresponding to the number
of balls we draw), and the event probabilities, $p$ (corresponding to
the proportion of different colored balls).

#### Seeding the random number generator

Even though we do want to draw a *random* sample, we sometimes want
our results to be reproducible: even though the numbers seem random,
if we were to run the program again, we might want it to use the
*same* sequence of "random" numbers.

In order to allow for the generation of such "reproducibly random"
numbers, we need to tell our sampling function *how* to generate the
random numbers. We can accomplish this through use of a NumPy
`RandomState` object, which is essentially a random number generator
object that can be passed around. It has most of the same functions as
`np.random`; the difference is that we get to control where the random
numbers come from.

```python
>>> import numpy as np
>>> rso = np.random.RandomState(230489)
```

where the number passed to the `RandomState` constructor is the *seed*
for the random number generator. As long as we instantiate it with the
same seed, a `RandomState` object will produce the same "random"
numbers in the same order, thus ensuring replicability.

> Aside: the functions in `np.random` actually do rely on a random
> number generator that we can control -- NumPy's global random number
> generator. You can set the global seed with `np.seed`. There's a
> tradeoff to using the global generator vs. a local `RandomState`
> object. If you use the global generator, then you don't have to pass
> around a `RandomState` object everywhere. However, you also run the
> risk of depending on some third party code that also uses the global
> generator without your knowledge. If you use a local object, then it
> is easier to find out whether there is nondeterminism coming from
> somewhere other than your own code.

#### Writing the sampling code

So, to actually implement a function that samples from the multinomial
distribution, we can use the multinomial sampler from NumPy, and pass
in a `RandomState` object for reproducibility. I like to make the
`RandomState` object an optional parameter: it is occasionally
convenient to not be *forced* to use it, but I do want to have the
*option* of using it (which, if I were to just use the `np.random`
function, I would not be able to do).

```python
import numpy as np

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
```

### Evaluating the multinomial PMF

Although we don't explicitly need to compute the probability of the
magical items that we generate, it is almost always a good idea to
write a function that can compute the distribution's probability mass
function (PMF) or probability density function (PDF). Why?

One reason is that we can use it for testing: if we take many samples
with our sampling function, then they should approximate the exact PDF
or PMF. If after many samples the approximation is poor or obviously
wrong, then we know there is a bug in our code somewhere.

Another reason to implement the PMF or PDF is because frequently, you
will actually need it later down the line and simply don't realize it
initially. For example, we might want to classify our randomly
generated items as *common*, *uncommon*, and *rare*, depending on how
likely it is to be generated. To determine this, we need to be able to
compute the PMF.

Finally, in many cases, your particular use case will dictate that you
implement the PMF or PDF from the beginning, anyway.

#### The multinomial PMF equation

Formally, the multinomial distribution has the following equation:

$$
p(\mathbf{x}; n, \mathbf{p}) = \frac{n!}{x_1!\cdots{}x_k!}p_1^{x_1}\cdots{}p_k^{x_k},
$$

where $\mathbf{x}=[x_1, \ldots{}, x_k]$ is a vector of length $k$
specifying the number of times each event happened, $n=\sum_{i=1}^k
x_i$ is the total number of events, and
$\mathbf{p}=[p_1, \ldots{}, p_k]$ is a vector specifying the
probability of each event occurring. As mentioned above, the variables
$n$ and $\mathbf{p}$ are the *parameters* of the distribution.

The factorials in the equation above can actually be expressed using a
special function, $\Gamma$, called the *gamma function*. When we get
to writing the code, it will be more convenient and efficient to use
the gamma function rather than factorial, so we will rewrite the
equation using $\Gamma$:

$$
p(\mathbf{x}; n, \mathbf{p}) = \frac{\Gamma(n+1)}{\Gamma(x_1+1)\cdots{}\Gamma(x_k+1)}p_1^{x_1}\cdots{}p_k^{x_k},
$$

#### Working in "log-space"

Before getting into the actual code needed to implement the equation
above, I want to emphasize one of the *the most important design
decisions* when writing code with probabilities: working in
"log-space". What this means is that rather than working directly with
probabilities $p(x)$, we should be working with *log*-probabilities,
$\log{p(x)}$. This is because probabilities can get very small very
quickly, resulting in underflow errors.

To motivate this, consider that probabilities must range between 0 and
1 (inclusive). NumPy has a useful function, `finfo`, that will tell us
the limits of floating point values for our system. For example, on a
64-bit machine, we see that the smallest usable positive number (given
by `tiny`) is:

```python
>>> import numpy as np
>>> np.finfo(float).tiny
2.2250738585072014e-308
```

While that may seem very small, it is not unusual to encounter
probabilities of this magnitude, or even smaller! Moreover, it is a
common operation to multiply probabilities, yet if we try to do this
with very small probabilities, we encounter underflow problems:

```python
>>> tiny = np.finfo(float).tiny
>>> # if we multiply numbers that are too small, we lose all precision
>>> tiny * tiny
0.0
```

However, taking the log can help alleviate this issue because we can
represent a much wider range of numbers in log-space than we can
normally. Officially, log-space ranges from $-\infty$ to zero. In
practice, log-space ranges from the `min` value returned by `finfo`,
which is the smallest number that can be represented, to zero. The
`min` value is *much* smaller than the log of the `tiny` value (which
would be our lower bound if we did not work in log space):

```python
>>> # this is our lower bound normally
>>> np.log(tiny)
-708.39641853226408
>>> # this is our lower bound in log-space
>>> np.finfo(float).min
-1.7976931348623157e+308
```

So, by working in log-space, we can greatly expand our range of
representable numbers.

Moreover, we can perform multiplication in log-space using addition,
 because of the identity that $\log(x\cdot{}y) = \log(x) +
 \log(y)$. Thus, if we do the multiplication above in log-space, we do
 not have to worry (as much) about loss of precision due to underflow:

```python
>>> # the result of multiplying small probabilities
>>> np.log(tiny * tiny)
-inf
>>> # the result of adding small log probabilities
>>> np.log(tiny) + np.log(tiny)
-1416.7928370645282
```

Of course, this solution is not a magic bullet. If we need to bring
the number *out* of log-space (for example, to add probabilities,
rather than multiply them), then we are back to the issue of
underflow:

```python
>>> tiny*tiny
0.0
>>> np.exp(np.log(tiny) + np.log(tiny))
0.0
```

Still, doing all our computations in log-space can save a lot of
headache. We might be forced to lose that precision if we need to go
out of log-space, but we at least maintain *some* information about
the probabilities -- enough to compare them, for example -- that would
otherwise be lost.

#### Writing the PMF code

TODO

```python
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
```

If we really do need the PMF, and not the log-PMF, we can still
compute it. However, it is generally better to *first* compute it in
log-space, and then exponentiate it if we need to take it out of
log-space:

```python
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
```

To further drive home the point of why working in log-space is so
important, we can look at an example just with the multinomial:

```python
>>> multinomial_logpmf(np.array([1000, 0, 0, 0]), np.array([0.25, 0.25, 0.25, 0.25]))
-1386.2943611198905
>>> multinomial_logpmf(np.array([999, 0, 0, 0]), np.array([0.25, 0.25, 0.25, 0.25]))
-1384.9080667587707
```

In this case, we get *extremely* small probabilities (which, you will
notice, are much smaller than the `tiny` value we discussed
above). This is because the denominator in the PMF is huge: 1000
factorial can't even be computed due to overflow. But, the *log* of
the factorial can be:

```python
>>> from scipy.special import gamma, gammaln
>>> gamma(1000 + 1)
inf
>>> gammaln(1000 + 1)
5912.1281784881639
```

## Sampling magical items, revisited

Now that we have written our multinomial functions, we can put them to
work to actually generate our magical items! To do this, we will
create a class called `MagicItemSampler`, located in the file
`sampler.py`.

The constructor to our `MagicItemSampler` class takes parameters for
the bonus probabilities, the stats probabilities, and the random
number generator. Even though we specified above what we wanted the
bonus probabilities to be, it is generally a good idea to encode
parameters as arguments that are passed in. This leaves open the
possibility of sampling items under different distributions (for
example, maybe the bonus probabilities would change as the player's
level increases). We encode the *names* of the stats as a class
variable, `stats_names`, though this could just as easily be another
parameter to the constructor.

As mentioned previously, there are two steps to sampling a magical
item: first sampling the overall bonus, and then sampling the
distribution of the bonus across the stats. As such, we code these
steps as two methods: `_sample_bonus` and `_sample_stats`. We *could*
have made these be just a single method--especially since
`_sample_stats` is the only function that depends on
`_sample_bonus`--but I have chosen to keep them separate, both because
it makes the sampling routine easier to understand, and because
breaking it up into smaller pieces makes the code easier to test.

You'll also notice that these methods are prefixed with an underscore,
indicating that they're not really meant to be used outside the
class. Instead, we provide the function `sample`, which does
essentially the same thing as `_sample_stats`, except that it returns
a dictionary with the stats names as keys. This provides a clean and
understandable interface for sampling items--it is obvious which stats
have how many bonus points--but it also keeps the option open for
using just `_sample_stats` if one needs to take many samples and
efficiency is required.

We use a similar design for evaluating the probability of
items. Again, we expose high-level methods `pmf` and `logpmf` which
take dictionaries of the form produced by `sample`. These methods rely
on `_bonus_logpmf`, which computes the probability of the overall
bonus, and `_stats_logpmf`, which computes the probability of the
stats (but which takes an array rather than a dictionary).

We can now create our sampler as follows:

```python
>>> import numpy as np
>>> from sampler import MagicItemSampler
>>> bonus_probs = np.array([0.0, 0.55, 0.25, 0.12, 0.06, 0.02])
>>> stats_probs = np.ones(6) / 6.0
>>> item_sampler = MagicItemSampler(bonus_probs, stats_probs)
```

Once created, we can use it to generate a few different items:

```python
>>> item_sampler.sample()
{'dexterity': 0, 'strength': 0, 'constitution': 0, 'intelligence': 0,
'wisdom': 1, 'charisma': 0}
>>> item_sampler.sample()
{'dexterity': 0, 'strength': 0, 'constitution': 2, 'intelligence': 1,
'wisdom': 0, 'charisma': 0}
>>> item_sampler.sample()
{'dexterity': 0, 'strength': 0, 'constitution': 1, 'intelligence': 0,
'wisdom': 0, 'charisma': 0}
```

And, if we want, we can evaluate the probability of a sampled item:

```python
>>> item = item_sampler.sample()
>>> item
{'dexterity': 0, 'strength': 0, 'constitution': 0, 'intelligence': 0,
'wisdom': 1, 'charisma': 0}
>>> item_sampler.logpmf(item)
-2.3895964699836751
>>> item_sampler.pmf(item)
0.091666666666666688
```

## Uses of sampling

Ok, now that we've constructed a way to randomly generate magical
items, we might want to use this to compute answers to some
questions. For example, let's say that damage in our RPG works by
rolling some number of D12s (twelve-sided dice). The player gets to
roll one die by default, and then add dice according to their strength
bonus. So, for example, if they have a +2 strength bonus, they can
roll three dice. The damage dealt is then the sum of the dice.

We might want to specify a way to assign hit points to monsters based
on the current ability level of the player. For example, let's say the
player has one weapon (but we don't know what the stats bonuses on
them are), and we want the player to be able to defeat the monster
within three hits about half the time. How many hit points should the
monster have?

One way to answer this question is through sampling. We can use the
following scheme:

1. Randomly pick a magic item.
2. Based on the item's bonuses, compute the number of dice that will
   be rolled when attacking.
3. Based on the number of dice that will be rolled, generate a sample
   for the damage inflicted over three hits.
4. Repeat steps 1-3 many times. This will result in an approximation
   to the distribution over damage.
5. Compute the expected amount of damage dealt by finding the mean of
   all the samples.

```
from sampler import MagicItemSampler
bonus_probs = np.array([0.0, 0.55, 0.25, 0.12, 0.06, 0.02])
stats_probs = np.ones(6) / 6.0
stats_names = ("dexterity", "constitution", "strength", "intelligence", "wisdom", "charisma")
s = MagicItemSampler(stats_names, bonus_probs, stats_probs)
item = s.sample()
dice = 1 + s['strength']
dice_probs = np.ones(12) / 12.0
damage = np.sum(np.arange(1, 13, 1) * sample_multinomial(3, dice_probs))




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
