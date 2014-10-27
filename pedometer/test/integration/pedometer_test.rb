require 'test/unit'
require 'rack/test'
require_relative '../../pedometer'

class PedometerTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_uploads
    get '/uploads'

    assert_equal 200, last_response.status
  end

  def test_upload
    get '/upload/test/data/female-167-70_1-100-10-bagwalk.txt'
    
    assert_equal 200, last_response.status
  end

  def test_create
    # Create an upload
    post '/create', {
      'data'  => { 'tempfile' => 'test/data/upload-1.txt' }, 
      'trial' => { 'name' => 'foo', 'rate' => '100', 'steps' => '10', 'method' => 'run' }, 
      'user'  => { 'gender' => 'female', 'height' => '157', 'stride' => '90' }
    }
    
    assert_equal 302, last_response.status

    # Retrieve the upload and ensure data is as expected
    upload = Upload.find('public/uploads/female-157.0-90.0_foo-100-10-run.txt')
    assert_equal 'female', upload.user.gender
    assert_equal 157, upload.user.height
    assert_equal 90, upload.user.stride

    assert_equal 100, upload.trial.rate
    assert_equal 10, upload.trial.steps
    assert_equal 'foo', upload.trial.name
    assert_equal 'run', upload.trial.method
    
    assert_equal 103, upload.analyzer.steps

    rm('public/uploads/female-157.0-90.0_foo-100-10-run.txt')
  end

end