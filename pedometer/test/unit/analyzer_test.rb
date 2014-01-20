require 'test/unit'
require './models/analyzer.rb'
require './models/device_data.rb'

class AnalyzerTest < Test::Unit::TestCase

  # -- Creation Tests -------------------------------------------------------

  def test_create_accelerometer_data
    user = User.new
    input = '0.123,-0.123,5;'
    device_data = DeviceData.new(input)
    analyzer = Analyzer.new(device_data, user)
    
    assert_equal device_data, analyzer.device_data
    assert_equal 0, analyzer.steps
    assert_equal 0, analyzer.distance
    assert_equal 0, analyzer.time
    assert_equal 'seconds', analyzer.interval
    assert_equal user, analyzer.user
  end

  def test_create_gravity_data
    user = User.new
    input = '0.028,-0.072,5|0.129,-0.945,-5;'
    device_data = DeviceData.new(input)
    analyzer = Analyzer.new(device_data, user)
    
    assert_equal device_data, analyzer.device_data
    assert_equal 0, analyzer.steps
    assert_equal 0, analyzer.distance
    assert_equal 0, analyzer.time
    assert_equal 'seconds', analyzer.interval
    assert_equal user, analyzer.user
  end

  def test_create_no_device_data
    message = "Input data must be of type DeviceData."
    assert_raise_with_message(RuntimeError, message) do
      Analyzer.new(nil)
    end
  end

  def test_create_bad_device_data
    message = "Input data must be of type DeviceData."
    assert_raise_with_message(RuntimeError, message) do
      Analyzer.new('bad device data')
    end
  end

  def test_create_no_user
    device_data = DeviceData.new('0.123,-0.123,5;')
    analyzer = Analyzer.new(device_data)
    assert analyzer.user.kind_of? User
  end

  def test_create_bad_user
    device_data = DeviceData.new('0.123,-0.123,5;')
    analyzer = Analyzer.new(device_data, 'bad user')
    assert analyzer.user.kind_of? User
  end

  # -- Edge Detection Tests -------------------------------------------------

  def test_split_on_threshold
    device_data = DeviceData.new(File.read('test/data/female/walking-10-g-1.txt'))
    analyzer = Analyzer.new(device_data)

    expected = File.read('test/data/female/expected/walking-10-g-1-positive.txt').split(',').collect(&:to_i)
    assert_equal expected, analyzer.split_on_threshold(true)

    expected = File.read('test/data/female/expected/walking-10-g-1-negative.txt').split(',').collect(&:to_i)
    assert_equal expected, analyzer.split_on_threshold(false)
  end

  def test_detect_edges
    device_data = DeviceData.new(File.read('test/data/female/walking-10-g-1.txt'))
    analyzer = Analyzer.new(device_data)
    
    assert_equal 9, analyzer.detect_edges(analyzer.split_on_threshold(true))
    assert_equal 7, analyzer.detect_edges(analyzer.split_on_threshold(false))
  end

  def test_detect_edges_false_step
    device_data = DeviceData.new(File.read('test/data/female/walking-1-g-false-step.txt'))
    analyzer = Analyzer.new(device_data)
    
    assert_equal 1, analyzer.detect_edges(analyzer.split_on_threshold(true))
    assert_equal 1, analyzer.detect_edges(analyzer.split_on_threshold(false))    
  end

  # -- Measurement Tests ----------------------------------------------------

  def test_measure_steps
    device_data = DeviceData.new(File.read('test/data/results-0-steps.txt'))
    analyzer = Analyzer.new(device_data)
    analyzer.measure_steps
    assert_equal 0, analyzer.steps

    device_data = DeviceData.new(File.read('test/data/results-15-steps.txt'))
    analyzer = Analyzer.new(device_data)
    analyzer.measure_steps
    assert_equal 15, analyzer.steps

    device_data = DeviceData.new(File.read('test/data/female/walking-10-g-1.txt'))
    analyzer = Analyzer.new(device_data)
    analyzer.measure_steps
    assert_equal 8, analyzer.steps
  end

  def test_measure_distance_before_steps
    device_data = DeviceData.new(File.read('test/data/results-0-steps.txt'))
    analyzer = Analyzer.new(device_data)
    analyzer.measure_distance
    assert_equal 0, analyzer.distance
    
    device_data = DeviceData.new(File.read('test/data/results-15-steps.txt'))
    analyzer = Analyzer.new(device_data)
    analyzer.measure_distance
    assert_equal 0, analyzer.distance
  end

  def test_measure_distance_after_steps
    user = User.new(:stride => 65)

    device_data = DeviceData.new(File.read('test/data/results-0-steps.txt'))
    analyzer = Analyzer.new(device_data, user)
    analyzer.measure_steps
    analyzer.measure_distance
    assert_equal 0, analyzer.distance
    
    device_data = DeviceData.new(File.read('test/data/results-15-steps.txt'))
    analyzer = Analyzer.new(device_data, user)
    analyzer.measure_steps
    analyzer.measure_distance
    assert_equal 975, analyzer.distance
  end

  def test_measure_time_seconds
    user = User.new(:rate => 4)
    device_data = DeviceData.new(File.read('test/data/results-15-steps.txt'))
    analyzer = Analyzer.new(device_data, user)
    analyzer.measure_time
    
    assert_equal 7.25, analyzer.time
    assert_equal 'seconds', analyzer.interval
  end

  def test_measure_time_minutes
    user = User.new(:rate => 4)
    # Fake out 1000 samples
    device_data = DeviceData.new(1000.times.inject('') {|a| a+='1,1,1;';a})
    analyzer = Analyzer.new(device_data, user)    
    analyzer.measure_time
    
    assert_equal 4.17, analyzer.time
    assert_equal 'minutes', analyzer.interval
  end

  def test_measure_time_hours
    user = User.new(:rate => 4)
    # Fake out 15000 samples
    device_data = DeviceData.new(15000.times.inject('') {|a| a+='1,1,1;';a})
    analyzer = Analyzer.new(device_data, user)
    analyzer.measure_time

    assert_equal 1.04, analyzer.time
    assert_equal 'hours', analyzer.interval
  end

  def test_measure
    user = User.new(:stride => 65, :rate => 5)
    device_data = DeviceData.new(File.read('test/data/results-0-steps.txt'))
    analyzer = Analyzer.new(device_data, user)
    analyzer.measure

    assert_equal 0, analyzer.steps
    assert_equal 0, analyzer.distance
    assert_equal 0.2, analyzer.time
    assert_equal 'seconds', analyzer.interval
    
    device_data = DeviceData.new(File.read('test/data/results-15-steps.txt'))
    analyzer = Analyzer.new(device_data, user)
    analyzer.measure

    assert_equal 15, analyzer.steps
    assert_equal 975, analyzer.distance
    assert_equal 5.8, analyzer.time
    assert_equal 'seconds', analyzer.interval
  end

end