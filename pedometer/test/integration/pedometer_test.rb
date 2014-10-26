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
    post '/create', {
      'data'  => { 'tempfile' => 'test/data/female-167-70_1-100-10-bagwalk.txt' }, 
      'trial' => { 'name' => 'one', 'rate' => '100', 'steps' => '10', 'method' => 'run' }, 
      'user'  => { 'gender' => 'female', 'height' => '157', 'stride' => '90' }
    }
    
    assert_equal 302, last_response.status
  end

end