require 'test/unit'
require_relative '../../models/filter'

class FilterTest < Test::Unit::TestCase

  def test_filter_gravity
    data = [0.123, 0.456, -0.212]
    expected = [0, 0, 7.109485333219216e-05]

    assert_equal expected, Filter.low_0_hz(data)
  end

  def test_filter_smoothing
    data = [0.0, 0.0, 0.0005219529804999682]
    expected = [0, 0, 4.9828746074755684e-05]

    assert_equal expected, Filter.low_5_hz(data)
  end

  def test_filter_highpass
    data = [0, 0, 4.9828746074755684e-05]
    expected = [0, 0, 4.753597533351234e-05]

    assert_equal expected, Filter.high_1_hz(data)
  end

end
