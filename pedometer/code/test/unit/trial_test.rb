require 'test/unit'
require_relative '../../models/trial'

class TrialTest < Test::Unit::TestCase

  def test_create
    trial = Trial.new('walk 1 ', 5, '10')
    assert_equal 'walk1', trial.name
    assert_equal 5,       trial.rate
    assert_equal 10,      trial.steps
  end

  def test_create_empty_name
    [nil, '', ' '].each do |name|
      assert_raise_with_message(RuntimeError, 'Invalid name') do
        Trial.new name
      end
    end
  end

  def test_create_with_rate
    assert_nil Trial.new('walk1').rate
    assert_nil Trial.new('walk1', nil).rate
    assert_nil Trial.new('walk1', '').rate

    [0, '0', -1, '-1'].each do |rate|
      assert_raise_with_message(RuntimeError, 'Invalid rate') do
        Trial.new('walk1', rate)
      end
    end

    ['invalid rate', 2.5, '2.5'].each do |rate|
      assert_raises(ArgumentError) do
        Trial.new('walk1', rate)
      end
    end

    assert_equal 1,   Trial.new('walk1', 1).rate
    assert_equal 1,   Trial.new('walk1', '1').rate
    assert_equal 100, Trial.new('walk1', 100).rate
    assert_equal 100, Trial.new('walk1', '100').rate
  end

  def test_create_with_steps
    assert_nil Trial.new('walk1', nil).steps
    assert_nil Trial.new('walk1', nil, nil).steps
    assert_nil Trial.new('walk1', nil, '').steps

    [-1, '-1'].each do |steps|
      assert_raise_with_message(RuntimeError, 'Invalid steps') do
        Trial.new('walk1', nil, steps)
      end
    end

    ['invalid steps', 2.5, '2.5'].each do |steps|
      assert_raises(ArgumentError) do
        Trial.new('walk1', nil, steps)
      end
    end

    assert_equal 0,   Trial.new('walk1', nil, 0).steps
    assert_equal 0,   Trial.new('walk1', nil, '0').steps
    assert_equal 1,   Trial.new('walk1', nil, 1).steps
    assert_equal 1,   Trial.new('walk1', nil, '1').steps
    assert_equal 100, Trial.new('walk1', nil, 100).steps
    assert_equal 100, Trial.new('walk1', nil, '100').steps
  end

end
