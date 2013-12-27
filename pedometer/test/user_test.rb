require 'test/unit'
require 'user'

class UserTest < Test::Unit::TestCase

  def test_create_no_parameters
    user = User.new(nil)
    
    assert_nil user.gender
    assert_nil user.height
    assert_equal 74, user.stride
    assert_equal 'metric', user.system
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