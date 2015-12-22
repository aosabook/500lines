require 'test/unit'
require_relative '../../models/user'

class UserTest < Test::Unit::TestCase

  def test_create
    user = User.new('male', 167.5, 80)

    assert_equal 'male', user.gender
    assert_equal 167.5,  user.height
    assert_equal 80,     user.stride
  end

  def test_create_no_params
    user = User.new

    assert_nil user.gender
    assert_nil user.height
    assert_equal 74, user.stride
  end

  def test_create_with_gender
    assert_nil User.new.gender
    assert_nil User.new(nil).gender
    assert_nil User.new('').gender

    ['invalid gender', 1].each do |gender|
      assert_raise_with_message(RuntimeError, 'Invalid gender') do
        User.new(gender)
      end
    end

    assert_equal 'female', User.new('Female').gender
    assert_equal 'male',   User.new('MALE').gender
  end

  def test_create_with_height
    assert_nil User.new(nil).height
    assert_nil User.new(nil, nil).height
    assert_nil User.new(nil, '').height

    [0, '0', -1, '-1'].each do |height|
      assert_raise_with_message(RuntimeError, 'Invalid height') do
        User.new(nil, height)
      end
    end

    assert_raises(ArgumentError) do
      User.new(nil, 'invalid height')
    end

    assert_equal 150, User.new(nil, 150).height
    assert_equal 150, User.new(nil, '150').height
    assert_equal 150, User.new(nil, 150.0).height
    assert_equal 150, User.new(nil, '150.0').height
    assert_equal 167.5, User.new(nil, 167.5).height
    assert_equal 167.5, User.new(nil, '167.5').height
    assert_equal 181.12, User.new(nil, 181.12).height
    assert_equal 181.12, User.new(nil, '181.12').height
  end

  def test_create_with_stride
    assert_equal 74, User.new.stride
    assert_equal 74, User.new(nil, nil, nil).stride
    assert_equal 74, User.new(nil, nil, '').stride

    [0, '0', -1, '-1'].each do |stride|
      assert_raise_with_message(RuntimeError, 'Invalid stride') do
        User.new(nil, nil, stride)
      end
    end

    assert_raises(ArgumentError) do
      User.new(nil, nil, 'invalid stride')
    end

    assert_equal 80,    User.new(nil, nil, 80).stride
    assert_equal 80,    User.new(nil, nil, '80').stride
    assert_equal 80,    User.new(nil, nil, 80.0).stride
    assert_equal 80,    User.new(nil, nil, '80.0').stride
    assert_equal 75.25, User.new(nil, nil, 75.25).stride
    assert_equal 75.25, User.new(nil, nil, '75.25').stride
  end

  def test_create_with_height_and_gender
    assert_equal 0.415, User.new('male', 1).stride
    assert_equal 0.413, User.new('female', 1).stride
  end

  def test_calculate_stride
    assert_equal 74,   User.new.stride
    assert_equal 74,   User.new(nil).stride
    assert_equal 78,   User.new('male').stride
    assert_equal 70,   User.new('female').stride
    assert_equal 82.8, User.new(nil, 200).stride
    assert_equal 83,   User.new('male', 200).stride
    assert_equal 82.6, User.new('female', 200).stride
  end

end
