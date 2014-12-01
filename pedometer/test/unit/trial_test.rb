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

    ['invalid rate', 0, '0', -1, '-1', 2.5, '2.5'].each do |rate|
      assert_raise_with_message(RuntimeError, 'Invalid rate') do
        Trial.new(nil, nil, rate)
      end
    end

    assert_equal 1, Trial.new(nil, nil, 1).rate
    assert_equal 1, Trial.new(nil, nil, '1').rate
    assert_equal 100, Trial.new(nil, nil, 100).rate
    assert_equal 100, Trial.new(nil, nil, '100').rate
  end

  def test_create_with_steps
    assert_nil Trial.new(nil, nil, nil).steps
    assert_nil Trial.new(nil, nil, nil, nil).steps
    assert_nil Trial.new(nil, nil, nil, '').steps

    ['invalid steps', -1, '-1', 2.5, '2.5'].each do |steps|
      assert_raise_with_message(RuntimeError, 'Invalid steps') do
        Trial.new(nil, nil, nil, steps)
      end
    end

    assert_equal 0, Trial.new(nil, nil, nil, 0).steps
    assert_equal 0, Trial.new(nil, nil, nil, '0').steps
    assert_equal 1, Trial.new(nil, nil, nil, 1).steps
    assert_equal 1, Trial.new(nil, nil, nil, '1').steps
    assert_equal 100, Trial.new(nil, nil, nil, 100).steps
    assert_equal 100, Trial.new(nil, nil, nil, '100').steps
  end

end