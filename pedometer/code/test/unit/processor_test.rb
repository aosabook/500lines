require 'test/unit'
require_relative '../../models/parser'
require_relative '../../models/processor'

class ProcessorTest < Test::Unit::TestCase

  def test_new
    data = '0.123,-0.123,5;0.456,-0.789,0.111;-0.212,0.001,1;'
    parser = Parser.run(data)
    processor = Processor.new(parser.parsed_data)

    assert_nil processor.dot_product_data
    assert_nil processor.filtered_data
  end

  # -- Creation Tests -------------------------------------------------------

  def test_create_combined_data
    data = '0.123,-0.123,5;0.456,-0.789,0.111;-0.212,0.001,1;'
    parser = Parser.run(data)
    processor = Processor.run(parser.parsed_data)

    assert_equal [0.0, 0.0, 0.0005219529804999682], processor.dot_product_data
    assert_equal [0, 0, 4.753597533351234e-05], processor.filtered_data
  end

  def test_create_separated_data
    data = '0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;0.2,-1,2|0.1,-0.9,3;'
    parser = Parser.run(data)
    processor = Processor.run(parser.parsed_data)

    assert_equal [-24.928348, 0.36629, 6.92], processor.dot_product_data
    assert_equal [0, 0, -1.7004231121083724], processor.filtered_data
  end

end
