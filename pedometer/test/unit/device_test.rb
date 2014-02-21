require 'test/unit'
require './models/device.rb'

class DeviceTest < Test::Unit::TestCase

  def test_create
    input_data = '0.123,-0.123,5;'
    device = Device.new(:data => input_data, :rate => 5, :latency => 0.5)

    assert_equal input_data, device.data
    assert_equal 5, device.rate
    assert_equal 0.5, device.latency
    assert_nil device.method
    assert_nil device.steps
    assert_nil device.trial
  end

  def test_create_with_meta_data
    input_data = '0.123,-0.123,5;'
    
    device = Device.new(:data => input_data, :meta_data => "jogging,10,1")
    assert_equal 'jogging', device.method
    assert_equal 10, device.steps
    assert_equal '1', device.trial

    device = Device.new(:data => input_data, :meta_data => "jogging,foo,1")
    assert_equal 'jogging', device.method
    assert_equal 0, device.steps
    assert_equal '1', device.trial

    device = Device.new(:data => input_data, :meta_data => "jogging,1")
    assert_equal 'jogging', device.method
    assert_equal 1, device.steps
    assert_nil device.trial
  end

  def test_create_with_rate
    input = '0.123,-0.123,5;'

    assert_equal 100, Device.new(:data => input).rate
    assert_equal 100, Device.new(:data => input, :rate => nil).rate
    assert_equal 100, Device.new(:data => input, :rate => 'bad rate').rate
    assert_equal 100, Device.new(:data => input, :rate => 0).rate
    assert_equal 100, Device.new(:data => input, :rate => -1).rate
    
    assert_equal 2, Device.new(:data => input, :rate => '2').rate
    assert_equal 2, Device.new(:data => input, :rate => 2).rate
    assert_equal 2, Device.new(:data => input, :rate => 2.0).rate
    assert_equal 2, Device.new(:data => input, :rate => 1.7).rate
    assert_equal 1, Device.new(:data => input, :rate => 1.2).rate
  end

  def test_create_with_latency
    input = '0.123,-0.123,5;'

    assert_equal 0, Device.new(:data => input).latency
    assert_equal 0, Device.new(:data => input, :latency => nil).latency
    assert_equal 0, Device.new(:data => input, :latency => '').latency
    assert_equal 0, Device.new(:data => input, :latency => 0).latency
    assert_equal 0, Device.new(:data => input, :latency => -1).latency
    
    assert_equal 0.2, Device.new(:data => input, :latency => 0.2).latency
    assert_equal 0.3, Device.new(:data => input, :latency => '0.3').latency
    assert_equal 0.12345, Device.new(:data => input, :latency => 0.12345).latency
  end

  def test_create_accelerometer_data
    input = '0.123,-0.123,5;0.456,-0.789,0.111;-0.212,0.001,1;'
    device = Device.new(:data => input)
    assert_equal 1, device.format
  end

  def test_create_gravity_data
    input = '0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;0.2,-1,2|0.1,-0.9,3;'
    device = Device.new(:data => input)
    assert_equal 2, device.format
  end

  # -- Creation Failure Tests -----------------------------------------------

  def test_create_nil_input
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Device.new(:data => nil)
    end
  end

  def test_create_empty_input
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Device.new(:data => '')
    end
  end

  def test_create_bad_input_strings
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Device.new(:data => "0.123,-0.123,5;a,b,c;")
    end

    assert_raise_with_message(RuntimeError, message) do
      Device.new(:data => "0.028,-0.072,a|0.129,-0.945,-5;0,-0.07,0.06|b,-0.947,5;")
    end
  end

  def test_create_bad_input_too_many_values
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Device.new(:data => "0.123,-0.123,5;0.123,-0.123,5,9;")
    end

    assert_raise_with_message(RuntimeError, message) do
      Device.new(:data => "0.028,-0.072,5,6|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;")
    end
  end

  def test_create_bad_input_too_few_values
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Device.new(:data => "0.123,-0.123,5;0.123,-0.123;")
    end

    assert_raise_with_message(RuntimeError, message) do
      Device.new(:data => "0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947;")
    end
  end

end