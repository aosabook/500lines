require 'test/unit'
require 'pedometer'

class PedometerTest < Test::Unit::TestCase

  def test_create
    input = "x,y,z;0.123,-0.123,5;"
    pedometer = Pedometer.new(input)
    
    assert_equal 0, pedometer.steps
    assert_equal 0, pedometer.distance
    assert_equal input, pedometer.raw_data
    assert_equal [[0.123, 0.123, 5.0]], pedometer.parsed_data
  end

  def test_create_no_input
    flunk
  end

  def test_create_bad_input
    flunk
  end

  def test_measure_steps
    pedometer = Pedometer.new(File.read('test/data/results-0-steps.txt'))
    pedometer.measure_steps
    assert_equal 0, pedometer.steps

    pedometer = Pedometer.new(File.read('test/data/results-15-steps.txt'))
    pedometer.measure_steps
    assert_equal 15, pedometer.steps
  end

  def test_measure_distance_before_steps
    pedometer = Pedometer.new(File.read('test/data/results-0-steps.txt'))
    pedometer.measure_distance
    assert_equal 0, pedometer.distance
    
    pedometer = Pedometer.new(File.read('test/data/results-15-steps.txt'))
    pedometer.measure_distance
    assert_equal 0, pedometer.distance
  end

  def test_measure_distance_after_steps
    pedometer = Pedometer.new(File.read('test/data/results-0-steps.txt'))
    pedometer.measure_steps
    pedometer.measure_distance
    assert_equal 0, pedometer.distance
    
    pedometer = Pedometer.new(File.read('test/data/results-15-steps.txt'))
    pedometer.measure_steps
    pedometer.measure_distance
    assert_equal 13.5, pedometer.distance
  end

  def test_results
    pedometer = Pedometer.new(File.read('test/data/results-0-steps.txt'))
    assert_equal ({ :steps => 0, :distance => 0}), pedometer.results

    pedometer = Pedometer.new(File.read('test/data/results-15-steps.txt'))
    assert_equal ({ :steps => 15, :distance => 0.0135}), pedometer.results
  end

end