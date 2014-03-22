require 'test/unit'
require_relative '../../models/device'

class DeviceTest < Test::Unit::TestCase

  def test_create
    device = Device.new(5, 'walk', '10', 'test trial 1')
    assert_equal 5,              device.rate
    assert_equal 'walk',         device.method
    assert_equal 10,             device.steps
    assert_equal 'test trial 1', device.trial
  end

  def test_create_empty
    device = Device.new
    assert_equal 100, device.rate
    assert_nil device.method
    assert_nil device.steps
    assert_nil device.trial
  end

  def test_create_with_rate
    assert_equal 100, Device.new().rate
    assert_equal 100, Device.new(nil).rate
    assert_equal 100, Device.new('bad rate').rate
    assert_equal 100, Device.new(0).rate
    assert_equal 100, Device.new(-1).rate
    
    assert_equal 2, Device.new('2').rate
    assert_equal 2, Device.new(2).rate
    assert_equal 2, Device.new(2.0).rate
    assert_equal 2, Device.new(1.7).rate
    assert_equal 1, Device.new(1.2).rate
  end

  def test_create_with_steps
    assert_nil Device.new(nil, nil, 0).steps
    assert_nil Device.new(nil, nil, -1).steps
    assert_nil Device.new(nil, nil, '0').steps
    assert_nil Device.new(nil, nil, '-1').steps

    assert_equal 10, Device.new(nil, nil, 10).steps
    assert_equal 1,  Device.new(nil, nil, 0.5).steps
  end

end