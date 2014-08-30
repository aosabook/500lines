require 'test/unit'
require_relative '../../models/analyzer'

class AnalyzerTest < Test::Unit::TestCase

  PROCESSOR_MESSAGE = 'Processor invalid.'
  USER_MESSAGE   = 'User invalid.'
  TRIAL_MESSAGE = 'Trial invalid.'

  # -- Creation Tests -------------------------------------------------------

  def test_create_combined_data
    processor = Processor.new('0.123,-0.123,5;')
    user   = User.new
    trial = Trial.new
    
    analyzer = Analyzer.new(processor, user, trial)
    
    assert_equal processor, analyzer.processor
    assert_equal user,   analyzer.user
    assert_equal trial, analyzer.trial

    assert_nil analyzer.steps
    assert_nil analyzer.distance
    assert_nil analyzer.time
  end

  def test_create_separated_data
    processor = Processor.new('0.028,-0.072,5|0.129,-0.945,-5;')
    user   = User.new
    trial = Trial.new

    analyzer = Analyzer.new(processor, user, trial)
    
    assert_equal processor, analyzer.processor
    assert_equal user,   analyzer.user
    assert_equal trial, analyzer.trial

    assert_nil analyzer.steps
    assert_nil analyzer.distance
    assert_nil analyzer.time
  end

  def test_create_no_processor
    assert_raise_with_message(RuntimeError, PROCESSOR_MESSAGE) do
      Analyzer.new(nil)
    end
  end

  def test_create_no_user_no_trial
    processor = Processor.new('0.123,-0.123,5;')
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
      processor = Processor.new('0.123,-0.123,5;')
      analyzer = Analyzer.new(processor, 'bad user')
    end
  end

  def test_create_bad_trial
    assert_raise_with_message(RuntimeError, TRIAL_MESSAGE) do
      processor = Processor.new('0.123,-0.123,5;')
      analyzer = Analyzer.new(processor, User.new, 'bad trial')
    end
  end

  # -- Step Counting Tests --------------------------------------------------

  def test_count_basic
    processor = Processor.new(File.read('test/data/female-167-70_1-100-10-walk-g.txt'))
    analyzer = Analyzer.new(processor)
    analyzer.measure

    assert_equal 10, analyzer.steps
  end

  def test_count_false_step_period_too_short
    processor = Processor.new(File.read('test/data/female-167-70_1-100-0-walk-g.txt'))
    analyzer = Analyzer.new(processor)
    analyzer.measure

    assert_equal 0, analyzer.steps
  end

  def test_count_false_step_period_too_long
    flunk
  end

  def test_count_all_peaks_too_high
    processor = Processor.new(File.read('test/data/female-167-70_false1-100-0-walk-c.txt'))
    analyzer = Analyzer.new(processor)
    analyzer.measure

    assert_equal 0, analyzer.steps
  end

  def test_count_some_peaks_too_high
    processor = Processor.new(File.read('test/data/female-167-70_false2-100-1-walk-s.txt'))
    analyzer = Analyzer.new(processor)
    analyzer.measure

    assert_equal 1, analyzer.steps
  end

  # -- Measurement Tests ----------------------------------------------------

  def test_measure
    user = User.new(nil, nil, 100)
    processor = Processor.new(File.read('test/data/female-167-70_1-100-10-walk-g.txt'))
    analyzer = Analyzer.new(processor, user)
    analyzer.measure

    assert_equal 10,         analyzer.steps
    assert_equal 1000,       analyzer.distance
    assert_equal (1037/100), analyzer.time
  end

end