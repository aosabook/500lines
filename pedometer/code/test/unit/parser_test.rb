require 'test/unit'
require_relative '../../models/parser'

class ParserTest < Test::Unit::TestCase

  def test_new
    data = '0.123,-0.123,5;0.456,-0.789,0.111;-0.212,0.001,1;'
    parser = Parser.new(data)

    assert_nil parser.parsed_data
  end

  # -- Creation Tests -------------------------------------------------------

  def test_create_combined_data
    data = '0.123,-0.123,5;0.456,-0.789,0.111;-0.212,0.001,1;'
    parser = Parser.run(data)

    assert_equal [ [[0.123, -0.123, 5.0],   [0, 0, 0]],
                   [[0.456, -0.789, 0.111], [0, 0, 0]],
                   [[-0.2120710948533322, 0.0011468544965549535, 0.9994625125426089],
                    [7.109485333219216e-05, -0.00014685449655495343, 0.0005374874573911294]] ], parser.parsed_data
  end

  def test_create_separated_data
    data = '0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;0.2,-1,2|0.1,-0.9,3;'
    parser = Parser.run(data)

    assert_equal [ [[0.028, -0.072, 5], [0.129, -0.945, -5]],
                   [[0, -0.07, 0.06],   [0.123, -0.947, 5]],
                   [[0.2, -1.0, 2.0], [0.1, -0.9, 3.0]] ], parser.parsed_data
  end

  def test_create_string_values_parses_to_0s
    data = "1,2,foo;"
    parser = Parser.run(data)
    assert_equal [[[1.0, 2.0, 0.0], [0, 0, 0]]], parser.parsed_data

    data = "1,2,foo|4,bar,6;"
    parser = Parser.run(data)
    assert_equal [[[1.0, 2.0, 0.0], [4.0, 0.0, 6.0]]], parser.parsed_data
  end

  # -- Creation Failure Tests -----------------------------------------------

  def test_create_nil
    message = "Bad Input. Ensure data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Parser.run(nil)
    end
  end

  def test_create_empty
    message = "Bad Input. Ensure data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Parser.run('')
    end
  end

  def test_create_bad_input_too_many_values
    message = "Bad Input. Ensure data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Parser.run("0.123,-0.123,5;0.123,-0.123,5,9;")
    end

    assert_raise_with_message(RuntimeError, message) do
      Parser.run("0.028,-0.072,5,6|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;")
    end
  end

  def test_create_bad_input_too_few_values
    message = "Bad Input. Ensure data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Parser.run("0.123,-0.123,5;0.123,-0.123;")
    end

    assert_raise_with_message(RuntimeError, message) do
      Parser.run("0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947;")
    end
  end

  def test_create_bad_input_delimiters
    message = "Bad Input. Ensure data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Parser.run("1,2,3:4,5,6:")
    end

    message = "Bad Input. Ensure data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Parser.run("1,2,3;4:5,6;")
    end

    message = "Bad Input. Ensure data is properly formatted."
    assert_raise_with_message(RuntimeError, message) do
      Parser.run("1,2,3!4,5,6;")
    end
  end

end
