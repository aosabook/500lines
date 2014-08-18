require 'test/unit'
require_relative '../../models/upload'

class UploadTest < Test::Unit::TestCase

  def test_new_no_params
    assert_raise_with_message(RuntimeError, 'File name or input data must be passed in.') do
      Upload.new
    end
  end

  def test_create
    upload = Upload.create(
      'test/data/upload-1.txt',
      ['female', '168', '70'],
      ['1', '100', '100','walk']
    )
    
    assert_equal File.read('test/data/upload-1.txt'), upload.processor.data

    assert_equal 'female', upload.user.gender
    assert_equal 168, upload.user.height
    assert_equal 70, upload.user.stride

    assert_equal 100, upload.trial.rate
    assert_equal 100, upload.trial.steps
    assert_equal '1', upload.trial.name
    assert_equal 'walk', upload.trial.method
    
    assert_equal 101, upload.analyzer.steps
  end

  def test_find
    upload = Upload.find('public/uploads/female-168.0-70.0_1-100-100-walk-c.txt')
    
    assert_equal File.read('public/uploads/female-168.0-70.0_1-100-100-walk-c.txt'), upload.processor.data

    assert_equal 'female', upload.user.gender
    assert_equal 168, upload.user.height
    assert_equal 70, upload.user.stride

    assert_equal 100, upload.trial.rate
    assert_equal 100, upload.trial.steps
    assert_equal '1', upload.trial.name
    assert_equal 'walk', upload.trial.method
    
    assert_equal 101, upload.analyzer.steps
  end

  def test_all
    uploads = Upload.all
    assert (uploads.count > 0)
    assert_equal [Upload], uploads.map { |t| t.class }.uniq
  end

end