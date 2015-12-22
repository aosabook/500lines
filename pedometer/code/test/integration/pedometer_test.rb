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
    get '/upload/test/data/female-167-70_bagwalk1-100-10.txt'
    assert_equal 200, last_response.status
  end

  def test_create
    post '/create', {
      'data'  => { 'tempfile' => 'test/data/upload-1.txt' },
      'trial' => { 'name' => 'foo', 'rate' => '100', 'steps' => '10' },
      'user'  => { 'gender' => 'female', 'height' => '157', 'stride' => '90' }
    }

    assert_equal 302, last_response.status
    rm('public/uploads/female-157.0-90.0_foo-100-10.txt')
  end

end
