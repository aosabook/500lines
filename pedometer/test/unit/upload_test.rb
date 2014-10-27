require 'test/unit'
require_relative '../../models/parser'
require_relative '../../models/upload'

class UploadTest < Test::Unit::TestCase

  def test_create
    temp_file = 'test/data/upload-1.txt'
    parser = Parser.run(File.read(temp_file))
    processor = Processor.run(parser.parsed_data)
    analyzer = Analyzer.run(processor, User.new, Trial.new('bar'))

    upload = Upload.create(temp_file, analyzer)

    assert_equal File.read('public/uploads/--74_bar-100--.txt'), File.read(temp_file)

    rm('public/uploads/--74_bar-100--.txt')
  end

  def test_find
    upload = Upload.find('public/uploads/female-168.0-70.0_1-100-100-walk.txt')

    assert_equal ["female", "168.0", "70.0"], upload.user_params
    assert_equal ["1", "100", "100", "walk"], upload.trial_params
  end

  def test_all
    uploads = Upload.all
    assert (uploads.count > 0)
    assert_equal [Upload], uploads.map { |t| t.class }.uniq
  end

end