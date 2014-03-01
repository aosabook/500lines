require 'test/unit'
require './helpers/file_helper.rb'

class FileHelperTest < Test::Unit::TestCase

  def test_generate_file_name
    expected = 'a-b-c_d-e-f-gh-i'
    assert_equal expected, FileHelper.generate_file_name('a', 'b', 'c', 'd', 'e', 'f', 'g h', 'iiiii')

    expected = '--_----'
    assert_equal expected, FileHelper.generate_file_name(nil, nil, nil, nil, nil, nil, nil, nil)
  end

  def test_parse_file_name

  end

end