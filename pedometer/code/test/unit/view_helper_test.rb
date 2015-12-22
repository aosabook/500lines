require 'test/unit'
require_relative '../../helpers/view_helper'

class ViewHelperTest < Test::Unit::TestCase
  include ViewHelper

  def test_format_distance_cm
    assert_equal '0.01 cm',  format_distance(0.01)
    assert_equal '1.0 cm',   format_distance(1)
    assert_equal '99.99 cm', format_distance(99.987)
    assert_equal '1.0 m',    format_distance(99.999)
  end

  def test_format_distance_m
    assert_equal '1.0 m',    format_distance(100)
    assert_equal '999.99 m', format_distance(99998.99)
    assert_equal '1.0 km',   format_distance(99999.99)
  end

  def test_format_distance_km
    assert_equal '1.0 km',  format_distance(100000)
    assert_equal '1.99 km', format_distance(199000)
  end

  def test_format_time_nil
    assert_equal '', format_time(nil)
  end

  def test_format_time_seconds
    assert_equal '0 hr, 0 min, 1 sec',  format_time(1)
    assert_equal '0 hr, 0 min, 59 sec', format_time(59.4)
    assert_equal '0 hr, 1 min, 0 sec',  format_time(59.9)
  end

  def test_format_time_minutes
    assert_equal '0 hr, 1 min, 0 sec',   format_time(60)
    assert_equal '0 hr, 59 min, 59 sec', format_time(3599.4)
    assert_equal '1 hr, 0 min, 0 sec',   format_time(3599.9)
  end

  def test_format_time_hours
    assert_equal '1 hr, 0 min, 0 sec',    format_time(3600)
    assert_equal '1 hr, 5 min, 0 sec',    format_time(3900)
    assert_equal '17 hr, 46 min, 39 sec', format_time(9999999999)
  end

  def test_limit_1000_series_nil
    assert_equal [], limit_1000(nil)
  end

  def test_limit_1000_series_empty
    assert_equal [], limit_1000([])
  end

  def test_limit_1000_series_500
    series = (0..500).to_a
    assert_equal series, limit_1000(series)
  end

  def test_limit_1000_series_2000
    series = (0..2000).to_a
    assert_equal series[0..999], limit_1000(series)
  end

end
