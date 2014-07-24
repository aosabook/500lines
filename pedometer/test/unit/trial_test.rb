require 'test/unit'
require_relative '../../models/trial'

class TrialTest < Test::Unit::TestCase

  def test_new_no_params
    assert_raise_with_message(RuntimeError, 'File name or input data must be passed in.') do
      Trial.new
    end
  end

  def test_create
    trial = Trial.create(
      'test/data/trial-1.txt',
      ['female', '168', '70'],
      ['100', '100', '1','walk']
    )
    
    assert_equal File.read('test/data/trial-1.txt'), trial.processor.data

    assert_equal 'female', trial.user.gender
    assert_equal 168, trial.user.height
    assert_equal 70, trial.user.stride

    assert_equal 100, trial.device.rate
    assert_equal 100, trial.device.steps
    assert_equal '1', trial.device.trial
    assert_equal 'walk', trial.device.method
    
    assert_equal 101, trial.analyzer.steps
  end

  def test_find
    trial = Trial.find('public/uploads/female-168.0-70.0_100-100-1-walk-c.txt')
    
    assert_equal File.read('public/uploads/female-168.0-70.0_100-100-1-walk-c.txt'), trial.processor.data

    assert_equal 'female', trial.user.gender
    assert_equal 168, trial.user.height
    assert_equal 70, trial.user.stride

    assert_equal 100, trial.device.rate
    assert_equal 100, trial.device.steps
    assert_equal '1', trial.device.trial
    assert_equal 'walk', trial.device.method
    
    assert_equal 101, trial.analyzer.steps
  end

  def test_all
    trials = Trial.all
    assert (trials.count > 0)
    assert_equal [Trial], trials.map { |t| t.class }.uniq
  end

end