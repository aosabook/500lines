require 'test/unit'
require_relative '../../helpers/file_helper'

class FileHelperTest < Test::Unit::TestCase

  def test_generate_file_name
    expected = 'public/uploads/male-167.5-80.0_20-10-testtrial1-walk-a.txt'
    user = User.new('male', 167.5, 80)
    device = Device.new(20, '10', 'test trial 1', 'walk')
    parser = Parser.new('0.123,-0.123,5;')
    assert_equal expected, FileHelper.generate_file_name(parser, user, device)
  end

  def test_parse_file_name
    expected = [['a', 'b', 'c'],  ['d', 'e', 'f', 'g']]
    assert_equal expected, FileHelper.parse_file_name('public/uploads/a-b-c_d-e-f-g-h')
  end

  def test_parse_file_name_nil
    assert_raise_with_message(RuntimeError, 'file_name cannot be nil.') do
      FileHelper.parse_file_name(nil)
    end
  end

end