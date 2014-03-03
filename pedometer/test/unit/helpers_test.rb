require 'test/unit'
require './helpers/file_helper.rb'
require './helpers/hash_helper.rb'

class HelpersTest < Test::Unit::TestCase

  def test_generate_file_name
    expected = 'male-167.5-80.0_20-walk-10-testtrial1-a'
    user = User.new(gender: 'male', height: 167.5, stride: 80)
    device = Device.new(data: '0.123,-0.123,5;', rate: 20, method: 'walk', steps: '10', trial: 'test trial 1')
    assert_equal expected, FileHelper.generate_file_name(user, device)
  end

  def test_parse_file_name
    expected = {user: {gender: "a", height: "b", stride: "c"}, 
                device: {rate: "d", method: "e", steps: "f", trial: "g"}}
    assert_equal expected, FileHelper.parse_file_name('public/uploads/a-b-c_d-e-f-g-h')
  end

  def test_symbolize_keys
    assert_equal ({a: 'foo', b: 'bar'}), {'a' => 'foo', :b => 'bar'}.symbolize_keys
  end

end