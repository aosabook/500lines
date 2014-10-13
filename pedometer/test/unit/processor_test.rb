require 'test/unit'
require_relative '../../models/processor'

class ProcessorTest < Test::Unit::TestCase

  # -- Creation Tests -------------------------------------------------------

  def test_create_combined_data
    data = '0.123,-0.123,5;0.456,-0.789,0.111;-0.212,0.001,1;'
    processor = Processor.new(data)
    
    assert_equal [ [[0.123, -0.123, 5.0],   [0, 0, 0]],
                   [[0.456, -0.789, 0.111], [0, 0, 0]],
                   [[-0.2120710948533322, 0.0011468544965549535, 0.9994625125426089], 
                    [7.109485333219216e-05, -0.00014685449655495343, 0.0005374874573911294]] ], processor.parsed_data
    assert_equal [0.0, 0.0, 0.0005219529804999682], processor.dot_product_data
    assert_equal [0, 0, 4.753597533351234e-05], processor.filtered_data
  end

  def test_create_separated_data
    data = '0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;0.2,-1,2|0.1,-0.9,3;'
    processor = Processor.new(data)
    
    assert_equal [ [[0.028, -0.072, 5], [0.129, -0.945, -5]], 
                   [[0, -0.07, 0.06],   [0.123, -0.947, 5]],
                   [[0.2, -1.0, 2.0], [0.1, -0.9, 3.0]] ], processor.parsed_data
    assert_equal [-24.928348, 0.36629, 6.92], processor.dot_product_data
    assert_equal [0, 0, -1.7004231121083724], processor.filtered_data
  end

  def test_create_string_values_parses_to_0s
    data = "1,2,foo;"
    processor = Processor.new(data)
    assert_equal [[[1.0, 2.0, 0.0], [0, 0, 0]]], processor.parsed_data

    data = "1,2,foo|4,bar,6;"
    processor = Processor.new(data)
    assert_equal [[[1.0, 2.0, 0.0], [4.0, 0.0, 6.0]]], processor.parsed_data
  end

  # -- Creation Failure Tests -----------------------------------------------

  def test_create_nil
    message = "Bad Input. Ensure data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Processor.new(nil)
    end
  end

  def test_create_empty
    message = "Bad Input. Ensure data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Processor.new('')
    end
  end

  def test_create_bad_input_too_many_values
    message = "Bad Input. Ensure data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Processor.new("0.123,-0.123,5;0.123,-0.123,5,9;")
    end

    assert_raise_with_message(RuntimeError, message) do
      Processor.new("0.028,-0.072,5,6|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;")
    end
  end

  def test_create_bad_input_too_few_values
    message = "Bad Input. Ensure data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Processor.new("0.123,-0.123,5;0.123,-0.123;")
    end

    assert_raise_with_message(RuntimeError, message) do
      Processor.new("0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947;")
    end
  end

  def test_create_bad_input_delimiters
    message = "Bad Input. Ensure data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Processor.new("1,2,3:4,5,6:")
    end

    message = "Bad Input. Ensure data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Processor.new("1,2,3;4:5,6;")
    end

    message = "Bad Input. Ensure data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Processor.new("1,2,3!4,5,6;")
    end
  end

end