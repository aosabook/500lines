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
    expected = {:user => {:gender=>"a", :height=>"b", :stride=>"c"}, 
                :device=>{:rate=>"d", :method=>"e", :steps=>"f", :trial=>"g"}}
    assert_equal expected, FileHelper.parse_file_name('a-b-c_d-e-f-g-h')
  end

end