require 'test/unit'
require './models/user.rb'

class UserTest < Test::Unit::TestCase

  def test_create_no_params
    user = User.new
    
    assert_nil user.gender
    assert_nil user.height
    assert_equal 74, user.stride
    assert_equal 'metric', user.system
    assert_equal 5, user.rate
  end

  def test_create_bad_params
    user = User.new('bad params')
    
    assert_nil user.gender
    assert_nil user.height
    assert_equal 74, user.stride
    assert_equal 'metric', user.system
    assert_equal 5, user.rate
  end

  def test_create_with_rate
    assert_equal 5, User.new(:rate => nil).rate
    assert_equal 5, User.new(:rate => 'bad rate').rate
    assert_equal 5, User.new(:rate => 0).rate
    assert_equal 5, User.new(:rate => -1).rate
    
    assert_equal 2, User.new(:rate => 2).rate
    assert_equal 2, User.new(:rate => 2.0).rate
    assert_equal 2, User.new(:rate => 1.7).rate
    assert_equal 1, User.new(:rate => 1.2).rate
  end

  def test_create_with_system
    assert_equal 'metric',   User.new(:system => 'MetrIC').system
    assert_equal 'imperial', User.new(:system => 'imperial').system

    assert_equal 'metric', User.new(:system => 'invalid value').system
    assert_equal 'metric', User.new(:system => 1).system
  end

  def test_create_with_gender
    assert_equal 'female', User.new(:gender => 'Female').gender
    assert_equal 'male',   User.new(:gender => 'MALE').gender
    
    assert_nil User.new(:gender => 'invalid value').gender
    assert_nil User.new(:gender => 1).gender
  end

  def test_create_with_stride
    assert_equal 74, User.new(:stride => nil).stride
    assert_equal 74, User.new(:stride => 'bad stride').stride
    assert_equal 74, User.new(:stride => 30).stride
    assert_equal 74, User.new(:stride => -1).stride
    
    assert_equal 80,    User.new(:stride => 80).stride
    assert_equal 75.25, User.new(:stride => 75.25).stride
    assert_equal 75.56, User.new(:stride => 75.5555).stride
  end

  def test_calculate_stride
    assert_equal 74,   User.new.stride
    assert_equal 74,   User.new(nil).stride
    assert_equal 78,   User.new(:gender => 'male').stride
    assert_equal 70,   User.new(:gender => 'female').stride
    assert_equal 83,   User.new(:height => 200).stride
    assert_equal 83,   User.new(:gender => 'male', :height => 200).stride
    assert_equal 82.6, User.new(:gender => 'female', :height => 200).stride
  end

end