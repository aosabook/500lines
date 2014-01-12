require 'test/unit'
require './models/pedometer.rb'
require './models/device_data.rb'

class PedometerTest < Test::Unit::TestCase

  # -- Creation Tests -------------------------------------------------------

  def test_create_accelerometer_data
    user = User.new
    input = '0.123,-0.123,5;'
    device_data = DeviceData.new(input)
    pedometer = Pedometer.new(device_data, user)
    
    assert_equal device_data, pedometer.device_data
    assert_equal 0, pedometer.steps
    assert_equal 0, pedometer.distance
    assert_equal 0, pedometer.time
    assert_equal 'seconds', pedometer.interval
    assert_equal user, pedometer.user
  end

  def test_create_gravity_data
    user = User.new
    input = '0.028,-0.072,5|0.129,-0.945,-5;'
    device_data = DeviceData.new(input)
    pedometer = Pedometer.new(device_data, user)
    
    assert_equal device_data, pedometer.device_data
    assert_equal 0, pedometer.steps
    assert_equal 0, pedometer.distance
    assert_equal 0, pedometer.time
    assert_equal 'seconds', pedometer.interval
    assert_equal user, pedometer.user
  end

  def test_create_no_device_data
    message = "Input data must be of type DeviceData."
    assert_raise_with_message(RuntimeError, message) do
      Pedometer.new(nil)
    end
  end

  def test_create_bad_device_data
    message = "Input data must be of type DeviceData."
    assert_raise_with_message(RuntimeError, message) do
      Pedometer.new('bad device data')
    end
  end

  def test_create_no_user
    device_data = DeviceData.new('0.123,-0.123,5;')
    pedometer = Pedometer.new(device_data)
    assert pedometer.user.kind_of? User
  end

  def test_create_bad_user
    device_data = DeviceData.new('0.123,-0.123,5;')
    pedometer = Pedometer.new(device_data, 'bad user')
    assert pedometer.user.kind_of? User
  end

  # -- Edge Detection Tests -------------------------------------------------

  def test_split_on_threshold
    device_data = DeviceData.new(File.read('test/data/walking-10-g-1.txt'))
    pedometer = Pedometer.new(device_data)

    expected = File.read('test/data/expected/walking-10-g-1-positive.txt').split(',').collect(&:to_i)
    assert_equal expected, pedometer.split_on_threshold(true)

    expected = File.read('test/data/expected/walking-10-g-1-negative.txt').split(',').collect(&:to_i)
    assert_equal expected, pedometer.split_on_threshold(false)
  end

  def test_detect_edges
    device_data = DeviceData.new(File.read('test/data/walking-10-g-1.txt'))
    pedometer = Pedometer.new(device_data)
    
    assert_equal 10, pedometer.detect_edges(pedometer.split_on_threshold(true))
    assert_equal 7, pedometer.detect_edges(pedometer.split_on_threshold(false))
  end

  # -- Measurement Tests ----------------------------------------------------

  def test_measure_steps
    device_data = DeviceData.new(File.read('test/data/results-0-steps.txt'))
    pedometer = Pedometer.new(device_data)
    pedometer.measure_steps
    assert_equal 0, pedometer.steps

    device_data = DeviceData.new(File.read('test/data/results-15-steps.txt'))
    pedometer = Pedometer.new(device_data)
    pedometer.measure_steps
    assert_equal 15, pedometer.steps

    device_data = DeviceData.new(File.read('test/data/walking-10-g-1.txt'))
    pedometer = Pedometer.new(device_data)
    pedometer.measure_steps
    assert_equal 9, pedometer.steps
  end

  def test_measure_distance_before_steps
    device_data = DeviceData.new(File.read('test/data/results-0-steps.txt'))
    pedometer = Pedometer.new(device_data)
    pedometer.measure_distance
    assert_equal 0, pedometer.distance
    
    device_data = DeviceData.new(File.read('test/data/results-15-steps.txt'))
    pedometer = Pedometer.new(device_data)
    pedometer.measure_distance
    assert_equal 0, pedometer.distance
  end

  def test_measure_distance_after_steps
    user = User.new(:stride => 65)

    device_data = DeviceData.new(File.read('test/data/results-0-steps.txt'))
    pedometer = Pedometer.new(device_data, user)
    pedometer.measure_steps
    pedometer.measure_distance
    assert_equal 0, pedometer.distance
    
    device_data = DeviceData.new(File.read('test/data/results-15-steps.txt'))
    pedometer = Pedometer.new(device_data, user)
    pedometer.measure_steps
    pedometer.measure_distance
    assert_equal 975, pedometer.distance
  end

  def test_measure_time_seconds
    user = User.new(:rate => 4)
    device_data = DeviceData.new(File.read('test/data/results-15-steps.txt'))
    pedometer = Pedometer.new(device_data, user)
    pedometer.measure_time
    
    assert_equal 7.25, pedometer.time
    assert_equal 'seconds', pedometer.interval
  end

  def test_measure_time_minutes
    user = User.new(:rate => 4)
    # Fake out 1000 samples
    device_data = DeviceData.new(1000.times.inject('') {|a| a+='1,1,1;';a})
    pedometer = Pedometer.new(device_data, user)    
    pedometer.measure_time
    
    assert_equal 4.17, pedometer.time
    assert_equal 'minutes', pedometer.interval
  end

  def test_measure_time_hours
    user = User.new(:rate => 4)
    # Fake out 15000 samples
    device_data = DeviceData.new(15000.times.inject('') {|a| a+='1,1,1;';a})
    pedometer = Pedometer.new(device_data, user)
    pedometer.measure_time

    assert_equal 1.04, pedometer.time
    assert_equal 'hours', pedometer.interval
  end

  def test_measure
    user = User.new(:stride => 65)
    device_data = DeviceData.new(File.read('test/data/results-0-steps.txt'))
    pedometer = Pedometer.new(device_data, user)
    pedometer.measure

    assert_equal 0, pedometer.steps
    assert_equal 0, pedometer.distance
    assert_equal 0.2, pedometer.time
    assert_equal 'seconds', pedometer.interval
    
    device_data = DeviceData.new(File.read('test/data/results-15-steps.txt'))
    pedometer = Pedometer.new(device_data, user)
    pedometer.measure

    assert_equal 15, pedometer.steps
    assert_equal 975, pedometer.distance
    assert_equal 5.8, pedometer.time
    assert_equal 'seconds', pedometer.interval
  end

end