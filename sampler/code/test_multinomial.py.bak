import numpy as np
import pytest
from multinomial import MultinomialDistribution


def test_init_without_rso():
    """Initialize without rso"""
    p = np.array([0.1, 0.5, 0.3, 0.1])
    dist = MultinomialDistribution(p)
    assert (dist.p == p).all()
    assert (dist.logp == np.log(p)).all()
    assert dist.rso is np.random


def test_init_with_rso():
    """Initialize with rso"""
    p = np.array([0.1, 0.5, 0.3, 0.1])
    rso = np.random.RandomState(29348)
    dist = MultinomialDistribution(p, rso=rso)
    assert (dist.p == p).all()
    assert (dist.logp == np.log(p)).all()
    assert dist.rso == rso


def test_init_bad_probabilities():
    """Initialize with probabilities that don't sum to 1"""
    p = np.array([0.1, 0.5, 0.3, 0.0])
    rso = np.random.RandomState(29348)
    with pytest.raises(ValueError):
        MultinomialDistribution(p, rso=rso)


def test_pmf_1():
    """Test PMF with only one possible event"""
    p = np.array([1.0])
    rso = np.random.RandomState(29348)
    dist = MultinomialDistribution(p, rso=rso)
    assert dist.pmf(np.array([1])) == 1.0
    assert dist.pmf(np.array([2])) == 1.0
    assert dist.pmf(np.array([10])) == 1.0


def test_pmf_2():
    """Test PMF with two possible events, one with zero probability"""
    p = np.array([1.0, 0.0])
    rso = np.random.RandomState(29348)
    dist = MultinomialDistribution(p, rso=rso)
    assert dist.pmf(np.array([1, 0])) == 1.0
    assert dist.pmf(np.array([0, 1])) == 0.0
    assert dist.pmf(np.array([2, 0])) == 1.0
    assert dist.pmf(np.array([2, 2])) == 0.0
    assert dist.pmf(np.array([10, 0])) == 1.0
    assert dist.pmf(np.array([10, 3])) == 0.0

    p = np.array([0.0, 1.0])
    rso = np.random.RandomState(29348)
    dist = MultinomialDistribution(p, rso=rso)
    assert dist.pmf(np.array([0, 1])) == 1.0
    assert dist.pmf(np.array([1, 0])) == 0.0
    assert dist.pmf(np.array([0, 2])) == 1.0
    assert dist.pmf(np.array([2, 2])) == 0.0
    assert dist.pmf(np.array([0, 10])) == 1.0
    assert dist.pmf(np.array([3, 10])) == 0.0


def test_pmf_3():
    """Test PMF with two possible events, both with nonzero probability"""
    p = np.array([0.5, 0.5])
    rso = np.random.RandomState(29348)
    dist = MultinomialDistribution(p, rso=rso)
    assert dist.pmf(np.array([1, 0])) == 0.5
    assert dist.pmf(np.array([0, 1])) == 0.5
    assert dist.pmf(np.array([2, 0])) == 0.25
    assert dist.pmf(np.array([0, 2])) == 0.25
    assert dist.pmf(np.array([1, 1])) == 0.5


def test_sample_1():
    """Test sampling with only one possible event"""
    p = np.array([1.0])
    rso = np.random.RandomState(29348)
    dist = MultinomialDistribution(p, rso=rso)
    samples = np.array([dist.sample(1) for i in xrange(100)])
    assert samples.shape == (100, 1)
    assert (samples == 1).all()
    samples = np.array([dist.sample(3) for i in xrange(100)])
    assert samples.shape == (100, 1)
    assert (samples == 3).all()


def test_sample_2():
    """Test sampling with two possible events, one with zero probability"""
    p = np.array([1.0, 0.0])
    rso = np.random.RandomState(29348)
    dist = MultinomialDistribution(p, rso=rso)
    samples = np.array([dist.sample(1) for i in xrange(100)])
    assert samples.shape == (100, 2)
    assert (samples == np.array([1, 0])).all()
    samples = np.array([dist.sample(3) for i in xrange(100)])
    assert samples.shape == (100, 2)
    assert (samples == np.array([3, 0])).all()

    p = np.array([0.0, 1.0])
    rso = np.random.RandomState(29348)
    dist = MultinomialDistribution(p, rso=rso)
    samples = np.array([dist.sample(1) for i in xrange(100)])
    assert samples.shape == (100, 2)
    assert (samples == np.array([0, 1])).all()
    samples = np.array([dist.sample(3) for i in xrange(100)])
    assert samples.shape == (100, 2)
    assert (samples == np.array([0, 3])).all()


def test_sample_3():
    """Test sampling with two possible events, both with nonzero probability"""
    p = np.array([0.5, 0.5])
    rso = np.random.RandomState(29348)
    dist = MultinomialDistribution(p, rso=rso)
    samples = np.array([dist.sample(1) for i in xrange(100)])
    assert samples.shape == (100, 2)
    assert ((samples == np.array([1, 0])) |
            (samples == np.array([0, 1]))).all()
    samples = np.array([dist.sample(3) for i in xrange(100)])
    assert samples.shape == (100, 2)
    assert ((samples == np.array([3, 0])) |
            (samples == np.array([2, 1])) |
            (samples == np.array([1, 2])) |
            (samples == np.array([0, 3]))).all()
