require 'test/unit'
require './helpers/file_helper.rb'
require './helpers/hash_helper.rb'

class HelpersTest < Test::Unit::TestCase

  def test_generate_file_name
    expected = 'male-167.5-80.0_20-walk-10-testtrial1-a'
    user = User.new('male', 167.5, 80)
    device = Device.new('0.123,-0.123,5;', 20, 'walk', '10', 'test trial 1')
    assert_equal expected, FileHelper.generate_file_name(user, device)
  end

  def test_parse_file_name
    expected = [['a', 'b', 'c'],  ['d', 'e', 'f', 'g']]
    assert_equal expected, FileHelper.parse_file_name('public/uploads/a-b-c_d-e-f-g-h')
  end

  def test_symbolize_keys
    assert_equal ({a: 'foo', b: 'bar'}), {'a' => 'foo', :b => 'bar'}.symbolize_keys
  end

end