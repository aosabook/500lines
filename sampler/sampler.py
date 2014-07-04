import numpy as np
from multinomial import sample_multinomial, multinomial_logpmf


# this is the minimum value we can exponentiate
MIN = np.log(np.finfo('float64').tiny)


class MagicItemSampler(object):

    # these are the names (and order) of the stats that all magical
    # items will have
    stats_names = ("dexterity", "constitution", "strength",
                   "intelligence", "wisdom", "charisma")

    def __init__(self, bonus_probs, stats_probs, rso=None):
        """Initialize the magic item sampler.

        Parameters
        ----------
        bonus_probs: numpy array of length m
            The probabilities of the overall bonuses. Each index in
            the array corresponds to the bonus of that amount (e.g.
            index 0 is +0, index 1 is +1, etc.)

        stats_probs: numpy array of length 6
            The probabilities of how the overall bonus is distributed
            among the different stats. `stats_probs[i]` corresponds to
            the probability of giving a bonus point to the ith stat,
            i.e. the value at `MagicItemSampler.stats_names[i]`.

        rso: numpy RandomState object (default: None)
            The random number generator

        """
        # Store the given parameters, in addition to the log of the
        # bonus probabilities (which are used by _bonus_logpmf).
        self.bonus_probs = bonus_probs
        self.bonus_log_probs = np.log(self.bonus_probs)
        self.stats_probs = stats_probs
        self.rso = rso

    def sample(self):
        """Sample a random magical item.

        Returns
        -------
        dictionary
            The keys are the names of the stats, and the values are
            the bonus conferred to the corresponding stat.

        """
        stats = self._sample_stats()
        item_stats = dict(zip(self.stats_names, stats))
        return item_stats

    def logpmf(self, item):

        """Compute the log probability the given magical item.

        Parameters
        ----------
        item: dictionary
            The keys are the names of the stats, and the values are
            the bonus conferred to the corresponding stat.

        Returns
        -------
        float
            The value corresponding to log(p(item))

        """
        # First pull out the bonus points for each stat, in the
        # correct order, then pass that to _stats_logpmf.
        stats = np.array([item[stat] for stat in self.stats_names])
        log_pmf = self._stats_logpmf(stats)
        return log_pmf

    def pmf(self, item):
        """Compute the probability the given magical item.

        Parameters
        ----------
        item: dictionary
            The keys are the names of the stats, and the values are
            the bonus conferred to the corresponding stat.

        Returns
        -------
        float
            The value corresponding to p(item)

        """
        return np.exp(self.logpmf(item))

    def _sample_bonus(self):
        """Sample a value of the overall bonus.

        Returns
        -------
        integer
            The overall bonus

        """
        # The bonus is essentially just a sample from a multinomial
        # distribution with n=1, i.e., only one event occurs.
        sample = sample_multinomial(1, self.bonus_probs, rso=self.rso)

        # `sample` is an array of zeros and a single one at the
        # location corresponding to the bonus. We want to convert this
        # one into the actual value of the bonus.
        bonus = np.argwhere(sample)[0, 0]
        return bonus

    def _sample_stats(self):
        """Sample the overall bonus and how it is distributed across the
        different stats.

        Returns
        -------
        numpy array of length 6
            The number of bonus points for each stat

        """
        # First we need to sample the overall bonus.
        bonus = self._sample_bonus()
        # Then, we use a different multinomial distribution to sample
        # how that bonus is distributed. The bonus corresponds to the
        # number of events.
        stats = sample_multinomial(bonus, self.stats_probs, rso=self.rso)
        return stats

    def _bonus_logpmf(self, bonus):
        """Evaluate the log-PMF for the given bonus.

        Parameters
        ----------
        bonus: integer
            The total bonus.

        Returns
        -------
        float
            The value corresponding to log(p(bonus))

        """
        # Make sure the value that is passed in is within the
        # appropriate bounds.
        if bonus < 0 or bonus >= len(self.bonus_probs):
            return -np.inf

        # Because the bonus is just a single event, we can just index
        # into the array of log probabilities, and do not actually
        # need to call multinomial_logpmf.
        return self.bonus_log_probs[bonus]

    def _stats_logpmf(self, stats):
        """Evaluate the log-PMF for the given distribution of bonus points
        across the different stats.

        Parameters
        ----------
        stats: numpy array of length 6
            The distribution of bonus points across the stats

        Returns
        -------
        float
            The value corresponding to log(p(stats))

        """
        # There are never any leftover bonus points, so the sum of the
        # stats gives us the total bonus.
        total_bonus = np.sum(stats)

        # First calculate the probability of the total bonus
        logp_bonus = self._bonus_logpmf(total_bonus)

        # Then calculate the probability of the stats
        logp_stats = multinomial_logpmf(stats, self.stats_probs)

        # Then multiply them together
        log_pmf = logp_bonus + logp_stats
        return log_pmf


class DamageSampler(object):

    def __init__(self, num_items, item_sampler,
                 num_dice_sides=12, num_hits=1, rso=None):
        """Initialize the damage sampler. This object will sample possible
        values for the attack damage dealt over `num_hits` hits when
        the player has `num_items` items, and where attack damage is
        computed by rolling dice with `num_dice_sides` sides.

        Parameters
        ----------
        num_items: int
            The number of items the player has.
        item_sampler: MagicItemSampler object
            A sampler for generating random items.
        num_dice_sides: int (default: 12)
            The number of sides on each die.
        num_hits: int (default: 1)
            The number of hits across which we want to calculate damage.
        rso: numpy RandomState object (default: None)
            The random number generator

        """
        # This is an array of integers corresponding to the sides of a
        # single die. The `dice_probs` are equal probabilities for
        # each of the sides.
        self.dice_sides = np.arange(1, num_dice_sides + 1)
        self.dice_probs = np.ones(num_dice_sides) / float(num_dice_sides)

        self.num_hits = num_hits
        self.num_items = num_items
        self.item_sampler = item_sampler
        self.rso = rso

    def sample(self):
        """Sample the attack damage.

        Returns
        -------
        int
            The sampled damage

        """
        # First, we need to randomly generate items (the number of
        # which was passed into the constructor).
        items = [self.item_sampler.sample() for i in xrange(self.num_items)]

        # Based on the item stats (in particular, strength), compute
        # the number of dice we get to roll.
        num_dice = 1 + np.sum([item['strength'] for item in items])

        # Roll the dice and compute the resulting damage.
        dice_rolls = sample_multinomial(
            self.num_hits * num_dice, self.dice_probs, rso=self.rso)
        damage = np.sum(self.dice_sides * dice_rolls)
        return damage


class RejectionSampler(object):

    def __init__(self, envelope_func, envelope_logpdf, target_logpdf):
        """Initialize the rejection sampler.

        Parameters
        ----------
        envelope_func : function
            The envelope distribution, q. Calling this function should
            return one sample, x, from this distribution.

        envelope_logpdf : function
            The envelope distribution's log probability density
            function (log-PDF). Calling this function with a sample
            from `envelope_func` should return the log probability
            density for the envelope distribution at that location,
            i.e. log(q(x)). This MUST be greater than the log-PDF of
            the target distribution, for any given input (i.e., q(x) >
            p(x) for all x)

        target_logpdf : function
            The target distribution's log probability density function
            (log-PDF). Calling this function with a sample from
            `envelope_func` should return the log probability density
            for the target distribution at that location,
            i.e. log(p(x)).

        """

        self.envelope_func = envelope_func
        self.envelope_logpdf = envelope_logpdf
        self.target_logpdf = target_logpdf

        self.samples = None

    def draw(self):
        """Sample a single value, x, from the target log probability density
        function (`self.target_logpdf`, or p) using rejection
        sampling.

        """

        while True:
            # 1. Sample a candidate value (x ~ q)
            candidate_sample = self.envelope_func()

            # 2. Sample a point uniformly between 0 and the PDF of the
            # envelope distribution (y ~ Uniform(0, q(x)))
            upper = self.envelope_logpdf(candidate_sample)
            if upper >= MIN:
                threshold = np.random.uniform(0.0, np.exp(upper))
                log_threshold = np.log(threshold)

                # 3. If this point is less than the target PDF (y <
                # p(x)), then we accept the candidate value;
                # otherwise, we reject it.
                if log_threshold < self.target_logpdf(candidate_sample):
                    break

        return candidate_sample

    def sample(self, n, seed=None):
        """Sample `n` values of x from the target log probability density
        function (`self.target_logpdf`, or p) using rejection
        sampling.

        Parameters
        ----------
        n : int
            The number of samples to draw.
        seed : int (optional)
            If given, seed NumPy's random number generator.

        """

        # seed the random number generator
        if seed is not None:
            np.random.seed(seed)

        # Draw our first sample, so we can determine the size of the
        # array that we need to allocate. If this sample is
        # array-like, then we need to allocate an array with more than
        # one dimension `d`.
        first = self.draw()
        try:
            d = len(first)
        except TypeError:
            # create an empty numpy array with shape (n,)
            self.samples = np.empty(n)
        else:
            # create an empty numpy array with shape (n, d)
            self.samples = np.empty((n, d))

        # Run the sampling loop for the number of requested samples.
        self.samples[0] = first
        for i in xrange(1, n):
            self.samples[i] = self.draw()

        return self.samples

    def plot(self, axes, x_min, x_max, y_max):
        """Plot the envelope distribution (q), target distribution (p), and
        normalized histogram of the samples (x) drawn by
        `self.sample`.

        Parameters
        ----------
        axes : matplotlib axes object
        x_min : number
            Minimum value for the x-axis
        x_max : number
            Maximum value for the x-axis
        y_max : number
            Maximum value for the y-axis

        """

        if self.samples is None:
            raise ValueError("no samples yet, please call `sample` first")
        if self.samples.ndim == 2 and self.samples.shape[1] > 1:
            raise ValueError(
                "visualization for dimensions great than 1 not supported")

        X = np.linspace(x_min, x_max, 1000)
        Yp = np.exp(np.array([self.envelope_logpdf(x) for x in X]))
        Yt = np.exp(np.array([self.target_logpdf(x) for x in X]))

        # plot the histogram of samples
        axes.hist(
            self.samples,
            bins=100,
            color='#999999',
            label="samples",
            normed=True,
            edgecolor='#999999')

        # plot the envelope distribution PDF
        axes.plot(
            X, Yp, 'r-',
            linewidth=2,
            label="envelope")

        # plot the target distribution PDF
        axes.plot(
            X, Yt, 'b-',
            linewidth=2,
            label="target")

        axes.legend(loc='upper right')
        axes.set_xlabel("x")
        axes.set_ylabel("p(x)")
        axes.set_xlim(x_min, x_max)
        axes.set_ylim(0, y_max)
