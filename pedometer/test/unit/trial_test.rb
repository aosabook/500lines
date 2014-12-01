require 'test/unit'
require_relative '../../models/trial'

class TrialTest < Test::Unit::TestCase

  def test_create
    trial = Trial.new('test trial 1', 'bag walk', 5, '10')
    assert_equal 'testtrial1', trial.name
    assert_equal 'bagwalk',    trial.method
    assert_equal 5,            trial.rate
    assert_equal 10,           trial.steps
  end

  def test_create_empty
    trial = Trial.new
    assert_equal '', trial.name
    assert_equal '', trial.method
    assert_equal 100, trial.rate
    assert_nil trial.steps
  end

  def test_create_with_rate
    assert_equal 100, Trial.new(nil, nil).rate
    assert_equal 100, Trial.new(nil, nil, nil).rate
    assert_equal 100, Trial.new(nil, nil, '').rate
    assert_equal 100, Trial.new(nil, nil, 'bad rate').rate
    
    assert_equal 2,   Trial.new(nil, nil, 2.0).rate
    assert_equal 1,   Trial.new(nil, nil, 1.7).rate
    assert_equal 100, Trial.new(nil, nil, 0.5).rate

    assert_equal 100, Trial.new(nil, nil, 0).rate
    assert_equal 100, Trial.new(nil, nil, '0').rate

    assert_equal 100, Trial.new(nil, nil, -1).rate
    assert_equal 100, Trial.new(nil, nil, '-1').rate
    
    assert_equal 2, Trial.new(nil, nil, '2').rate
    assert_equal 2, Trial.new(nil, nil, 2).rate
  end

  def test_create_with_steps
    assert_nil Trial.new(nil, nil, nil).steps
    assert_nil Trial.new(nil, nil, nil, nil).steps
    assert_nil Trial.new(nil, nil, nil, '').steps
    assert_nil Trial.new(nil, nil, nil, 'bad steps').steps
    
    assert_equal 2, Trial.new(nil, nil, nil, 2.0).steps
    assert_equal 1, Trial.new(nil, nil, nil, 1.7).steps
    assert_equal 0, Trial.new(nil, nil, nil, 0.5).steps

    assert_equal 0, Trial.new(nil, nil, nil, 0).steps
    assert_equal 0, Trial.new(nil, nil, nil, '0').steps
    
    assert_nil Trial.new(nil, nil, nil, -1).steps
    assert_nil Trial.new(nil, nil, nil, '-1').steps

    assert_equal 2, Trial.new(nil, nil, nil, '2').steps
    assert_equal 2, Trial.new(nil, nil, nil, 2).steps
  end

end