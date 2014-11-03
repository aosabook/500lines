require 'test/unit'
require_relative '../../models/parser'
require_relative '../../models/processor'
require_relative '../../models/analyzer'

class AnalyzerTest < Test::Unit::TestCase

  def test_new
    user = User.new
    trial = Trial.new
    analyzer = Analyzer.new([0, 0], user, trial)
    
    assert_equal [0, 0], analyzer.data
    assert_equal user, analyzer.user
    assert_equal trial, analyzer.trial

    assert_nil analyzer.steps
    assert_nil analyzer.distance
    assert_nil analyzer.time
  end

  # -- Creation Tests -------------------------------------------------------

  def test_create
    data = [0, 0, 3.0950446845522207e-05, 8.888784491236883e-05, 
            0.00017675661757108235, 0.0003010710258273255, 
            0.0004670334044406543, 0.0006857659826903315]
    user = User.new
    trial = Trial.new
    analyzer = Analyzer.run(data, user, trial)
    
    assert_equal data, analyzer.data
    assert_equal user, analyzer.user
    assert_equal trial, analyzer.trial

    assert_nil analyzer.delta
    assert_equal 0, analyzer.steps
    assert_equal 0, analyzer.distance
    assert_equal 0, analyzer.time
  end

  def test_create_non_zero_data
    user = User.new(nil, nil, 100)
    trial = Trial.new('test trial 1', 100, 18, 'walk')
    parser = Parser.run(File.read('test/data/female-167-70_2-100-10-walk.txt'))
    processor = Processor.run(parser.parsed_data)
    analyzer = Analyzer.run(processor.filtered_data, user, trial)

    assert_equal 10,         analyzer.steps
    assert_equal -8,         analyzer.delta
    assert_equal 1000,       analyzer.distance
    assert_equal (1037/100), analyzer.time
  end

  # -- Creation Failure Tests -----------------------------------------------

  def test_create_no_user_no_trial
    parser = Parser.run('0.123,-0.123,5;')
    processor = Processor.run(parser.parsed_data)
    analyzer = Analyzer.new(processor.filtered_data)
    assert analyzer.user.kind_of? User
    assert analyzer.trial.kind_of? Trial
  end

  def test_create_bad_user
    assert_raise_with_message(RuntimeError, 'User invalid.') do
      parser = Parser.run('0.123,-0.123,5;')
      processor = Processor.run(parser.parsed_data)
      analyzer = Analyzer.new(processor.filtered_data, 'bad user')
    end
  end

  def test_create_bad_trial
    assert_raise_with_message(RuntimeError, 'Trial invalid.') do
      parser = Parser.run('0.123,-0.123,5;')
      processor = Processor.run(parser.parsed_data)
      analyzer = Analyzer.new(processor.filtered_data, User.new, 'bad trial')
    end
  end

end