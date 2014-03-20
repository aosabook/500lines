require 'test/unit'
require_relative '../../models/parser'

class ParserTest < Test::Unit::TestCase

  # -- Creation Tests -------------------------------------------------------

  def test_create_accelerometer_data

    device = Device.new('0.123,-0.123,5;0.456,-0.789,0.111;-0.212,0.001,1;')
    parser = Parser.new(device)
    
    assert_equal device, parser.device
    assert_equal [{:x=>0.123, :y=>-0.123, :z=>5.0, :xg=>0, :yg=>0, :zg=>0},
                  {:x=>0.456, :y=>-0.789, :z=>0.111, :xg=>0, :yg=>0, :zg=>0},
                  {:x=>-0.2120710948533322,
                   :y=>0.0011468544965549535,
                   :z=>0.9994625125426089,
                   :xg=>7.109485333219216e-05,
                   :yg=>-0.00014685449655495343,
                   :zg=>0.0005374874573911294}], parser.parsed_data
    assert_equal [0.0, 0.0, 0.0005219529804999682], parser.dot_product_data
    assert_equal [0, 0, 4.9828746074755684e-05], parser.filtered_data
  end

  def test_create_gravity_data
    device = Device.new('0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;0.2,-1,2|0.1,-0.9,3;')
    parser = Parser.new(device)
    
    assert_equal device, parser.device
    assert_equal [{:x => 0.028, :y => -0.072, :z =>5, :xg => 0.129, :yg => -0.945, :zg => -5}, 
                  {:x => 0, :y => -0.07, :z =>0.06, :xg => 0.123, :yg => -0.947, :zg => 5},
                  {:x => 0.2, :y => -1.0, :z => 2.0, :xg => 0.1, :yg => -0.9, :zg => 3.0}], parser.parsed_data
    assert_equal [-24.928348, 0.36629, 6.92], parser.dot_product_data
    assert_equal [0, 0, -1.7824384769309702], parser.filtered_data
  end

  # -- Creation Failure Tests -----------------------------------------------

  def test_create_nil_input
    message = "A Device object must be passed in."
    assert_raise_with_message(RuntimeError, message) do
      Parser.new(nil)
    end
  end

  def test_create_empty_input
    message = "A Device object must be passed in."
    assert_raise_with_message(RuntimeError, message) do
      Parser.new('')
    end
  end

end