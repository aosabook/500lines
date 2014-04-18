require 'test/unit'
require_relative 'parser1'

class ParserTest < Test::Unit::TestCase

  # -- Creation Tests -------------------------------------------------------

  def test_create_separated_data
    data = '0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;0.2,-1,2|0.1,-0.9,3;'
    parser = Parser.new(data)
    
    assert_equal data, parser.data
    assert_equal [{:x => 0.028, :y => -0.072, :z =>5, :xg => 0.129, :yg => -0.945, :zg => -5}, 
                  {:x => 0, :y => -0.07, :z =>0.06, :xg => 0.123, :yg => -0.947, :zg => 5},
                  {:x => 0.2, :y => -1.0, :z => 2.0, :xg => 0.1, :yg => -0.9, :zg => 3.0}], parser.parsed_data
  end

end