import numpy as np
from multinomial import MultinomialDistribution


class MagicItemDistribution(object):

    # these are the names (and order) of the stats that all magical
    # items will have
    stats_names = ("dexterity", "constitution", "strength",
                   "intelligence", "wisdom", "charisma")

    def __init__(self, bonus_probs, stats_probs, rso=np.random):
        """Initialize a magic item distribution parameterized by `bonus_probs`
        and `stats_probs`.

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
            i.e. the value at `MagicItemDistribution.stats_names[i]`.

        rso: numpy RandomState object (default: np.random)
            The random number generator

        """
        # Create the multinomial distributions we'll be using
        self.bonus_dist = MultinomialDistribution(bonus_probs, rso=rso)
        self.stats_dist = MultinomialDistribution(stats_probs, rso=rso)

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

    def log_pmf(self, item):
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
        # correct order, then pass that to _stats_log_pmf.
        stats = np.array([item[stat] for stat in self.stats_names])
        log_pmf = self._stats_log_pmf(stats)
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
        return np.exp(self.log_pmf(item))

    def _sample_bonus(self):
        """Sample a value of the overall bonus.

        Returns
        -------
        integer
            The overall bonus

        """
        # The bonus is essentially just a sample from a multinomial
        # distribution with n=1; i.e., only one event occurs.
        sample = self.bonus_dist.sample(1)

        # `sample` is an array of zeros and a single one at the
        # location corresponding to the bonus. We want to convert this
        # one into the actual value of the bonus.
        bonus = np.argmax(sample)
        return bonus

    def _sample_stats(self):
        """Sample the overall bonus and how it is distributed across the
        different stats.

        Returns
        -------
        numpy array of length 6
            The number of bonus points for each stat

        """
        # First we need to sample the overall bonus
        bonus = self._sample_bonus()

        # Then, we use a different multinomial distribution to sample
        # how that bonus is distributed. The bonus corresponds to the
        # number of events.
        stats = self.stats_dist.sample(bonus)
        return stats

    def _bonus_log_pmf(self, bonus):
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
        # appropriate bounds
        if bonus < 0 or bonus >= len(self.bonus_dist.p):
            return -np.inf

        # Convert the scalar bonus value into a vector of event
        # occurrences
        x = np.zeros(len(self.bonus_dist.p))
        x[bonus] = 1

        return self.bonus_dist.log_pmf(x)

    def _stats_log_pmf(self, stats):
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
        logp_bonus = self._bonus_log_pmf(total_bonus)

        # Then calculate the probability of the stats
        logp_stats = self.stats_dist.log_pmf(stats)

        # Then multiply them together (using addition, because we are
        # working with logs)
        log_pmf = logp_bonus + logp_stats
        return log_pmf


class DamageDistribution(object):

    def __init__(self, num_items, item_dist,
                 num_dice_sides=12, num_hits=1, rso=np.random):
        """Initialize a distribution over attack damage. This object can
        sample possible values for the attack damage dealt over
        `num_hits` hits when the player has `num_items` items, and
        where attack damage is computed by rolling dice with
        `num_dice_sides` sides.

        Parameters
        ----------
        num_items: int
            The number of items the player has.
        item_dist: MagicItemDistribution object
            The distribution over magic items.
        num_dice_sides: int (default: 12)
            The number of sides on each die.
        num_hits: int (default: 1)
            The number of hits across which we want to calculate damage.
        rso: numpy RandomState object (default: np.random)
            The random number generator

        """
        # This is an array of integers corresponding to the sides of a
        # single die.
        self.dice_sides = np.arange(1, num_dice_sides + 1)
        # Create a multinomial distribution corresponding to one of
        # these dice.  Each side has equal probabilities.
        self.dice_dist = MultinomialDistribution(
            np.ones(num_dice_sides) / float(num_dice_sides), rso=rso)

        self.num_hits = num_hits
        self.num_items = num_items
        self.item_dist = item_dist

    def sample(self):
        """Sample the attack damage.

        Returns
        -------
        int
            The sampled damage

        """
        # First, we need to randomly generate items (the number of
        # which was passed into the constructor).
        items = [self.item_dist.sample() for i in xrange(self.num_items)]

        # Based on the item stats (in particular, strength), compute
        # the number of dice we get to roll.
        num_dice = 1 + np.sum([item['strength'] for item in items])

        # Roll the dice and compute the resulting damage.
        dice_rolls = self.dice_dist.sample(self.num_hits * num_dice)
        damage = np.sum(self.dice_sides * dice_rolls)
        return damage
