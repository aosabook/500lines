require 'test/unit'
require_relative '../../models/parser'
require_relative '../../models/upload'

class UploadTest < Test::Unit::TestCase

  def test_generate_file_path
    user = User.new('male', 167.5, 80)
    trial = Trial.new('test trial 1', 5, '10', 'walk')
    expected = "public/uploads/male-167.5-80.0_testtrial1-5-10-walk.txt"
    assert_equal expected, Upload.generate_file_path(user, trial)

    user = User.new
    trial = Trial.new
    expected = "public/uploads/--74.0_-100--.txt"
    assert_equal expected, Upload.generate_file_path(user, trial)
  end

  def test_create
    temp_file = 'test/data/upload-1.txt'
    user = User.new
    trial = Trial.new('bar')

    upload = Upload.create(temp_file, user, trial)

    assert_equal File.read('public/uploads/--74.0_bar-100--.txt'), File.read(temp_file)

    rm('public/uploads/--74.0_bar-100--.txt')
  end

  def test_find
    file_path = 'public/uploads/female-168.0-70.0_1-100-100-walk.txt'
    upload = Upload.find(file_path)

    assert_equal file_path, upload.file_path
    
    assert_equal "female", upload.user.gender
    assert_equal 168.0, upload.user.height
    assert_equal 70.0, upload.user.stride

    assert_equal "1", upload.trial.name
    assert_equal 100, upload.trial.rate
    assert_equal 100, upload.trial.steps
    assert_equal "walk", upload.trial.method
  end

  def test_all
    uploads = Upload.all
    assert (uploads.count > 0)
    assert_equal [Upload], uploads.map { |t| t.class }.uniq
  end

end