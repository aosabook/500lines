require 'test/unit'
require_relative '../../models/analyzer'

class AnalyzerTest < Test::Unit::TestCase

  PROCESSOR_MESSAGE = 'Processor invalid.'
  USER_MESSAGE = 'User invalid.'
  TRIAL_MESSAGE = 'Trial invalid.'

  def test_new
    parser = Parser.run('0.123,-0.123,5;')
    processor = Processor.run(parser.parsed_data)
    user = User.new
    trial = Trial.new
    
    analyzer = Analyzer.new(processor, user, trial)
    
    assert_equal processor, analyzer.processor
    assert_equal user, analyzer.user
    assert_equal trial, analyzer.trial

    assert_nil analyzer.steps
    assert_nil analyzer.distance
    assert_nil analyzer.time
  end

  # -- Creation Tests -------------------------------------------------------

  def test_create_combined_data
    parser = Parser.run('0.123,-0.123,5;')
    processor = Processor.run(parser.parsed_data)
    user = User.new
    trial = Trial.new
    
    analyzer = Analyzer.run(processor, user, trial)
    
    assert_equal processor, analyzer.processor
    assert_equal user, analyzer.user
    assert_equal trial, analyzer.trial

    assert_equal 0, analyzer.steps
    assert_equal 0, analyzer.distance
    assert_equal 0, analyzer.time
  end

  def test_create_separated_data
    parser = Parser.run('0.028,-0.072,5|0.129,-0.945,-5;')
    processor = Processor.run(parser.parsed_data)
    user = User.new
    trial = Trial.new

    analyzer = Analyzer.run(processor, user, trial)
    
    assert_equal processor, analyzer.processor
    assert_equal user, analyzer.user
    assert_equal trial, analyzer.trial

    assert_equal 0, analyzer.steps
    assert_equal 0, analyzer.distance
    assert_equal 0, analyzer.time
  end

  def test_create_non_zero_data
    user = User.new(nil, nil, 100)
    parser = Parser.run(File.read('test/data/female-167-70_2-100-10-walk.txt'))
    processor = Processor.run(parser.parsed_data)
    analyzer = Analyzer.run(processor, user)

    assert_equal 10,         analyzer.steps
    assert_equal 1000,       analyzer.distance
    assert_equal (1037/100), analyzer.time
  end

  # -- Creation Failure Tests -----------------------------------------------

  def test_create_no_processor
    assert_raise_with_message(RuntimeError, PROCESSOR_MESSAGE) do
      Analyzer.new(nil)
    end
  end

  def test_create_no_user_no_trial
    parser = Parser.run('0.123,-0.123,5;')
    processor = Processor.run(parser.parsed_data)
    analyzer = Analyzer.new(processor)
    assert analyzer.user.kind_of? User
    assert analyzer.trial.kind_of? Trial
  end

  def test_create_bad_processor
    assert_raise_with_message(RuntimeError, PROCESSOR_MESSAGE) do
      Analyzer.new('bad processor')
    end
  end

  def test_create_bad_user
    assert_raise_with_message(RuntimeError, USER_MESSAGE) do
      parser = Parser.run('0.123,-0.123,5;')
      processor = Processor.run(parser.parsed_data)
      analyzer = Analyzer.new(processor, 'bad user')
    end
  end

  def test_create_bad_trial
    assert_raise_with_message(RuntimeError, TRIAL_MESSAGE) do
      parser = Parser.run('0.123,-0.123,5;')
      processor = Processor.run(parser.parsed_data)
      analyzer = Analyzer.new(processor, User.new, 'bad trial')
    end
  end

end