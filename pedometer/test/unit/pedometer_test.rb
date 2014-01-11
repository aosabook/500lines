require 'test/unit'
require './models/pedometer.rb'

class PedometerTest < Test::Unit::TestCase

  # -- Creation Tests -------------------------------------------------------

  def test_create_accelerometer_data
    user = User.new
    input = '0.123,-0.123,5;0.456,-0.789,0.111;'
    pedometer = Pedometer.new(input, user)
    
    assert_equal 0, pedometer.steps
    assert_equal 0, pedometer.distance
    assert_equal 0, pedometer.time
    assert_equal 'seconds', pedometer.interval
    assert_equal user, pedometer.user

    assert_equal input, pedometer.raw_data
    assert_equal [[0.123, 0.123, 5.0],[0.456,0.789,0.111]], pedometer.parsed_data
  end

  def test_create_gravity_data
    user = User.new
    input = '0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;'
    pedometer = Pedometer.new(input, user)
    
    assert_equal 0, pedometer.steps
    assert_equal 0, pedometer.distance
    assert_equal 0, pedometer.time
    assert_equal 'seconds', pedometer.interval
    assert_equal user, pedometer.user

    assert_equal input, pedometer.raw_data
    assert_equal [{:x => 0.028, :y => -0.072, :z =>5, :xg => 0.129, :yg => -0.945, :zg => -5}, 
                  {:x => 0, :y => -0.07, :z =>0.06, :xg => 0.123, :yg => -0.947, :zg => 5}], pedometer.parsed_data
  end

  def test_create_no_user
    pedometer = Pedometer.new('0.123,-0.123,5;')
    assert pedometer.user.kind_of? User
  end

  def test_create_bad_user
    pedometer = Pedometer.new('0.123,-0.123,5;', 'bad user')
    assert pedometer.user.kind_of? User
  end

  # -- Creation Failure Tests -----------------------------------------------

  def test_create_nil_input
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Pedometer.new(nil)      
    end
  end

  def test_create_empty_input
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Pedometer.new('')
    end
  end

  def test_create_bad_input_strings
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Pedometer.new("0.123,-0.123,5;a,b,c;")
    end

    assert_raise_with_message(RuntimeError, message) do
      Pedometer.new("0.028,-0.072,a|0.129,-0.945,-5;0,-0.07,0.06|b,-0.947,5;")
    end
  end

  def test_create_bad_input_too_many_values
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Pedometer.new("0.123,-0.123,5;0.123,-0.123,5,9;")
    end

    assert_raise_with_message(RuntimeError, message) do
      Pedometer.new("0.028,-0.072,5,6|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;")
    end
  end

  def test_create_bad_input_too_few_values
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Pedometer.new("0.123,-0.123,5;0.123,-0.123;")
    end

    assert_raise_with_message(RuntimeError, message) do
      Pedometer.new("0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947;")
    end
  end

  # -- Measurement Tests ----------------------------------------------------

  def test_all_measurement_tests
    flunk 'Look at all measurement tests. Currently only work with accelerometer data.'
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
    user = User.new(:stride => 65)
    
    pedometer = Pedometer.new(File.read('test/data/results-0-steps.txt'), user)
    pedometer.measure_steps
    pedometer.measure_distance
    assert_equal 0, pedometer.distance
    
    pedometer = Pedometer.new(File.read('test/data/results-15-steps.txt'), user)
    pedometer.measure_steps
    pedometer.measure_distance
    assert_equal 975, pedometer.distance
  end

  def test_measure_time_seconds
    user = User.new(:rate => 4)
    pedometer = Pedometer.new(File.read('test/data/results-15-steps.txt'), user)
    pedometer.measure_time
    
    assert_equal 7.25, pedometer.time
    assert_equal 'seconds', pedometer.interval
  end

  def test_measure_time_minutes
    user = User.new(:rate => 4)
    # Fake out 1000 samples
    pedometer = Pedometer.new(1000.times.inject('') {|a| a+='1,1,1;';a}, user)    
    pedometer.measure_time
    
    assert_equal 4.17, pedometer.time
    assert_equal 'minutes', pedometer.interval
  end

  def test_measure_time_hours
    user = User.new(:rate => 4)
    # Fake out 15000 samples
    pedometer = Pedometer.new(15000.times.inject('') {|a| a+='1,1,1;';a}, user)
    pedometer.measure_time

    assert_equal 1.04, pedometer.time
    assert_equal 'hours', pedometer.interval
  end

  def test_measure
    user = User.new(:stride => 65)
    pedometer = Pedometer.new(File.read('test/data/results-0-steps.txt'), user)
    pedometer.measure

    assert_equal 0, pedometer.steps
    assert_equal 0, pedometer.distance
    assert_equal 0.2, pedometer.time
    assert_equal 'seconds', pedometer.interval
    
    pedometer = Pedometer.new(File.read('test/data/results-15-steps.txt'), user)
    pedometer.measure

    assert_equal 15, pedometer.steps
    assert_equal 975, pedometer.distance
    assert_equal 5.8, pedometer.time
    assert_equal 'seconds', pedometer.interval
  end

end