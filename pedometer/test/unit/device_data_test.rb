require 'test/unit'
require './models/device_data.rb'

class DeviceDataTest < Test::Unit::TestCase

  # -- Creation Tests -------------------------------------------------------

  def test_create_accelerometer_data
    input = '0.123,-0.123,5;0.456,-0.789,0.111;'
    device_data = DeviceData.new(input)
    
    assert_equal input, device_data.raw_data
    assert_equal [[0.123, 0.123, 5.0],[0.456,0.789,0.111]], device_data.parsed_data
    assert_nil device_data.dot_product_data
    assert_nil device_data.filtered_data
  end

  def test_create_gravity_data
    input = '0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;0.2,-1,2|0.1,-0.9,3;'
    device_data = DeviceData.new(input)
    
    assert_equal input, device_data.raw_data
    assert_equal [{:x => 0.028, :y => -0.072, :z =>5, :xg => 0.129, :yg => -0.945, :zg => -5}, 
                  {:x => 0, :y => -0.07, :z =>0.06, :xg => 0.123, :yg => -0.947, :zg => 5},
                  {:x => 0.2, :y => -1.0, :z => 2.0, :xg => 0.1, :yg => -0.9, :zg => 3.0}], device_data.parsed_data
    assert_equal [-24.928348, 0.36629, 6.92], device_data.dot_product_data
    assert_equal [0, 0, -1.7824384769309702], device_data.filtered_data
  end

  # -- Creation Failure Tests -----------------------------------------------

  def test_create_nil_input
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      DeviceData.new(nil)
    end
  end

  def test_create_empty_input
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      DeviceData.new('')
    end
  end

  def test_create_bad_input_strings
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      DeviceData.new("0.123,-0.123,5;a,b,c;")
    end

    assert_raise_with_message(RuntimeError, message) do
      DeviceData.new("0.028,-0.072,a|0.129,-0.945,-5;0,-0.07,0.06|b,-0.947,5;")
    end
  end

  def test_create_bad_input_too_many_values
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      DeviceData.new("0.123,-0.123,5;0.123,-0.123,5,9;")
    end

    assert_raise_with_message(RuntimeError, message) do
      DeviceData.new("0.028,-0.072,5,6|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;")
    end
  end

  def test_create_bad_input_too_few_values
    message = "Bad Input. Ensure accelerometer or gravity data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      DeviceData.new("0.123,-0.123,5;0.123,-0.123;")
    end

    assert_raise_with_message(RuntimeError, message) do
      DeviceData.new("0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947;")
    end
  end

end