require 'test/unit'
require_relative '../../models/user'

class UserTest < Test::Unit::TestCase

  def test_create
    user = User.new('male', 167.5, 80)
    
    assert_equal 'male', user.gender
    assert_equal 167.5, user.height
    assert_equal 80, user.stride
  end

  def test_create_no_params
    user = User.new
    
    assert_nil user.gender
    assert_nil user.height
    assert_equal 74, user.stride
  end

  def test_create_bad_params
    user = User.new('bad params')
    
    assert_nil user.gender
    assert_nil user.height
    assert_equal 74, user.stride
  end

  def test_create_with_gender
    assert_equal 'female', User.new('Female').gender
    assert_equal 'male',   User.new('MALE').gender
    
    assert_nil User.new('invalid value').gender
    assert_nil User.new(1).gender
  end

  def test_create_with_height
    assert_equal 167.5, User.new(nil, '167.5').height
    assert_equal 100.52, User.new(nil, 100.52).height
    
    assert_nil User.new(nil, 'invalid value').height
    assert_nil User.new(nil, 0).height
    assert_nil User.new(nil, -1).height
  end

  def test_create_with_height_and_gender
    assert_equal 0.415, User.new('male', 1).stride
    assert_equal 0.413, User.new('female', 1).stride
  end

  def test_create_with_stride
    assert_equal 74, User.new.stride
    assert_equal 74, User.new(nil, nil, 'bad stride').stride
    assert_equal 74, User.new(nil, nil, -1).stride
    
    assert_equal 80,    User.new(nil, nil, '80').stride
    assert_equal 75.25, User.new(nil, nil, 75.25).stride
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

  def test_for_chapter
    User.new.stride
    # => 74
    User.new('Female').stride
    # => 70
    User.new(nil, '167.5').stride
    # => 69.345
    User.new('male', 191).stride
    # => 79.265
    User.new(nil, nil, '80').stride
    # => 80.0
    User.new('female', 1, '72').stride
    # => 72.0
  end

end