require 'test/unit'
require './models/user.rb'

class UserTest < Test::Unit::TestCase

  def test_create_no_parameters
    user = User.new(nil)
    
    assert_nil user.gender
    assert_nil user.height
    assert_equal 74, user.stride
    assert_equal 'metric', user.system
  end

  def test_create_with_gender
    assert_equal 'female', User.new(:gender => 'Female').gender
    assert_equal 'male',   User.new(:gender => 'MALE').gender
    
    assert_nil User.new(:gender => 'invalid value').gender
    assert_nil User.new(:gender => 1).gender
  end

  def test_create_with_system
    assert_equal 'metric',   User.new(:system => 'MetrIC').system
    assert_equal 'imperial', User.new(:system => 'imperial').system

    assert_equal 'metric', User.new(:system => 'invalid value').system
    assert_equal 'metric', User.new(:system => 1).system
  end

  def test_create_bad_input
    flunk
  end

  def test_calculate_stride
    assert_equal 74,   User.new(nil).stride
    assert_equal 78,   User.new(:gender => 'male').stride
    assert_equal 70,   User.new(:gender => 'female').stride
    assert_equal 83,   User.new(:height => 200).stride
    assert_equal 83,   User.new(:gender => 'male', :height => 200).stride
    assert_equal 82.6, User.new(:gender => 'female', :height => 200).stride
  end

end