require 'test/unit'
require './models/device.rb'

class DeviceTest < Test::Unit::TestCase

  def test_create
    input_data = '0.123,-0.123,5;'
    device = Device.new(input_data, 5, 0.5)

    assert_equal input_data, device.data
    assert_equal 5, device.rate
    assert_equal 0.5, device.latency
  end

  def test_create_with_rate
    input = '0.123,-0.123,5;'

    assert_equal 100, Device.new(input).rate
    assert_equal 100, Device.new(input, nil).rate
    assert_equal 100, Device.new(input, 'bad rate').rate
    assert_equal 100, Device.new(input, 0).rate
    assert_equal 100, Device.new(input, -1).rate
    
    assert_equal 2, Device.new(input, '2').rate
    assert_equal 2, Device.new(input, 2).rate
    assert_equal 2, Device.new(input, 2.0).rate
    assert_equal 2, Device.new(input, 1.7).rate
    assert_equal 1, Device.new(input, 1.2).rate
  end

  def test_create_with_latency
    input = '0.123,-0.123,5;'

    assert_equal 0, Device.new(input,1).latency
    assert_equal 0, Device.new(input,1,nil).latency
    assert_equal 0, Device.new(input,1,'').latency
    assert_equal 0, Device.new(input,1,0).latency
    assert_equal 0, Device.new(input,1,-1).latency
    
    assert_equal 0.2, Device.new(input,1,0.2).latency
    assert_equal 0.3, Device.new(input,1,'0.3').latency
    assert_equal 0.12345, Device.new(input,1,0.12345).latency
  end

  def test_create_accelerometer_data
    input = '0.123,-0.123,5;0.456,-0.789,0.111;-0.212,0.001,1;'
    device = Device.new(input)
    assert_equal 1, device.format
  end

  def test_create_gravity_data
    input = '0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;0.2,-1,2|0.1,-0.9,3;'
    device = Device.new(input)
    assert_equal 2, device.format
  end

  # -- Creation Failure Tests -----------------------------------------------

  def test_create_nil_input
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Device.new(nil)
    end
  end

  def test_create_empty_input
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Device.new('')
    end
  end

  def test_create_bad_input_strings
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Device.new("0.123,-0.123,5;a,b,c;")
    end

    assert_raise_with_message(RuntimeError, message) do
      Device.new("0.028,-0.072,a|0.129,-0.945,-5;0,-0.07,0.06|b,-0.947,5;")
    end
  end

  def test_create_bad_input_too_many_values
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Device.new("0.123,-0.123,5;0.123,-0.123,5,9;")
    end

    assert_raise_with_message(RuntimeError, message) do
      Device.new("0.028,-0.072,5,6|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;")
    end
  end

  def test_create_bad_input_too_few_values
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Device.new("0.123,-0.123,5;0.123,-0.123;")
    end

    assert_raise_with_message(RuntimeError, message) do
      Device.new("0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947;")
    end
  end

end