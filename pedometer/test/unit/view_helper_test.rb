require 'test/unit'
require_relative '../../helpers/view_helper'

class UIHelperTest < Test::Unit::TestCase

  def test_format_distance_cm
    assert_equal '1.0 cm', ViewHelper.format_distance(1)
    assert_equal '99.99 cm', ViewHelper.format_distance(99.987)
  end

  def test_format_distance_m
    assert_equal '1.0 m', ViewHelper.format_distance(99.999)
    assert_equal '999.99 m', ViewHelper.format_distance(99999.9)
  end

  def test_format_distance_km
    assert_equal '1 Km', ViewHelper.format_distance(99999.9)
    assert_equal '1 km', ViewHelper.format_distance(10000)
  end

  def test_format_time
  end

end