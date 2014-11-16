require 'test/unit'
require_relative '../../models/parser'
require_relative '../../models/upload'

class UploadTest < Test::Unit::TestCase

  def test_new_no_params
    assert_raise_with_message(RuntimeError, 'A file path or user and trial parameters must be provided.') do
      Upload.new
    end
  end

  def test_create
    temp_file = 'test/data/upload-1.txt'
    file_path = 'public/uploads/female-999.0-90.0_foo-89-10-run.txt'
    user_params = { 'gender' => 'female', 'height' => '999', 'stride' => '90' }
    trial_params = { 'name' => 'foo', 'rate' => '89', 'steps' => '10', 'method' => 'run' }

    upload = Upload.create(temp_file, user_params, trial_params)

    assert_equal file_path, upload.file_path
    assert_equal File.read(file_path), File.read(temp_file)

    assert_equal user_params['gender'], upload.user.gender
    assert_equal user_params['height'].to_f, upload.user.height
    assert_equal user_params['stride'].to_f, upload.user.stride
    
    assert_equal trial_params['name'], upload.trial.name
    assert_equal trial_params['rate'].to_i, upload.trial.rate
    assert_equal trial_params['steps'].to_i, upload.trial.steps
    assert_equal trial_params['method'], upload.trial.method

    rm(file_path)
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