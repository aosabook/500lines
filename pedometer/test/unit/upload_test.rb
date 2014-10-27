require 'test/unit'
require_relative '../../models/parser'
require_relative '../../models/upload'

class UploadTest < Test::Unit::TestCase

  def test_generate_file_path_temp
    user_params = ['male', 167.5, 80]
    trial_params = ['test trial 1', 5, '10', 'walk']
    expected = "public/uploads/male-167.5-80_testtrial1-5-10-walk.txt"
    assert_equal expected, Upload.generate_file_path_temp(user_params, trial_params)

    user_params = ['male', '167.5', '80']
    trial_params = ['test trial 1', '5', '10', 'walk']    
    assert_equal expected, Upload.generate_file_path_temp(user_params, trial_params)    
  end

  def test_generate_file_path
    parser = Parser.run(File.read('test/data/upload-1.txt'))
    processor = Processor.run(parser.parsed_data)
    user = User.new('male', 167.5, 80)
    trial = Trial.new('test trial 2', 5, '10', 'walk')

    analyzer = Analyzer.run(processor, user, trial)

    expected = "public/uploads/male-167.5-80.0_testtrial2-5-10-walk.txt"
    assert_equal expected, Upload.generate_file_path(analyzer)
  end

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
    file_path = 'public/uploads/female-168.0-70.0_1-100-100-walk.txt'
    upload = Upload.find(file_path)

    assert_equal file_path, upload.file_path
    assert_equal ["female", "168.0", "70.0"], upload.user_params
    assert_equal ["1", "100", "100", "walk"], upload.trial_params
  end

  def test_all
    uploads = Upload.all
    assert (uploads.count > 0)
    assert_equal [Upload], uploads.map { |t| t.class }.uniq
  end

end