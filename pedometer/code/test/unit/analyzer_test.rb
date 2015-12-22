require 'test/unit'
require_relative '../../models/parser'
require_relative '../../models/processor'
require_relative '../../models/analyzer'

class AnalyzerTest < Test::Unit::TestCase

  def test_new
    analyzer = Analyzer.new([0, 0], User.new, Trial.new('walk1'))

    assert_nil analyzer.steps
    assert_nil analyzer.distance
    assert_nil analyzer.time
  end

  # -- Creation Tests -------------------------------------------------------

  def test_create
    data = [0, 0, 3.0950446845522207e-05, 8.888784491236883e-05,
            0.00017675661757108235, 0.0003010710258273255,
            0.0004670334044406543, 0.0006857659826903315]
    analyzer = Analyzer.run(data, User.new, Trial.new('walk1'))

    assert_nil analyzer.delta
    assert_nil analyzer.time
    assert_equal 0, analyzer.steps
    assert_equal 0, analyzer.distance
  end

  def test_create_non_zero_data
    user = User.new('female', 167, 70)
    trial = Trial.new('walk 1', 100, 18)
    parser = Parser.run(File.read('test/data/female-167-70_walk2-100-10.txt'))
    processor = Processor.run(parser.parsed_data)
    analyzer = Analyzer.run(processor.filtered_data, user, trial)

    assert_equal 10,         analyzer.steps
    assert_equal -8,         analyzer.delta
    assert_equal 700,        analyzer.distance
    assert_equal (1037/100), analyzer.time
  end

end
