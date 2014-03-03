require './pedometer.rb'
require 'test/unit'
require 'rack/test'

class PedometerTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_data
    get '/data'

    assert_equal 200, last_response.status
  end

  def test_detail
    get '/detail/test/data/female/bag-10-a-1.txt'
    
    assert_equal 200, last_response.status
  end

  def test_create
    post '/create', {"device"=>{"file"=>{"tempfile" => "test/data/female/walking-10-g-1.txt"}, "rate" => "100", "steps"=>"10", "trial"=>"one", "method"=>"run"}, "user"=>{"gender"=>"female", "height"=>"157", "stride"=>"90"}}
    
    assert_equal 200, last_response.status
  end

end