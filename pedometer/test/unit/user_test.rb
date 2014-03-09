require 'test/unit'
require './models/user.rb'

class UserTest < Test::Unit::TestCase

  def test_create
    user = User.new(gender: 'male', height: 167.5, stride: 80)
    
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
    assert_equal 'female', User.new(:gender => 'Female').gender
    assert_equal 'male',   User.new(:gender => 'MALE').gender
    
    assert_nil User.new(:gender => 'invalid value').gender
    assert_nil User.new(:gender => 1).gender
  end

  def test_create_with_height
    assert_equal 167.5, User.new(:height => '167.5').height
    assert_equal 100.52, User.new(:height => 100.519).height
    
    assert_nil User.new(:height => 'invalid value').height
    assert_nil User.new(:height => 0).height
    assert_nil User.new(:height => -1).height
  end

  def test_create_with_stride
    assert_equal 74, User.new(:stride => nil).stride
    assert_equal 74, User.new(:stride => 'bad stride').stride
    assert_equal 74, User.new(:stride => -1).stride
    
    assert_equal 80,    User.new(:stride => 80).stride
    assert_equal 75.25, User.new(:stride => 75.25).stride
    assert_equal 75.56, User.new(:stride => 75.5555).stride
  end

  def test_create_with_height_and_gender
    assert_equal 0.42, User.new(:gender => 'male', :height => 1).stride
    assert_equal 0.41, User.new(:gender => 'female', :height => 1).stride
  end

  def test_calculate_stride
    assert_equal 74,   User.new.stride
    assert_equal 74,   User.new(nil).stride
    assert_equal 78,   User.new(:gender => 'male').stride
    assert_equal 70,   User.new(:gender => 'female').stride
    assert_equal 82.8, User.new(:height => 200).stride
    assert_equal 83,   User.new(:gender => 'male', :height => 200).stride
    assert_equal 82.6, User.new(:gender => 'female', :height => 200).stride
  end

end