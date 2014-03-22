require 'test/unit'
require 'rack/test'
require_relative '../../pedometer'

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
    get '/detail/test/data/female-167-70_100-bagwalk-10-1-a.txt'
    
    assert_equal 200, last_response.status
  end

  def test_create
    post '/create', {"parser" => {"file"=>{"tempfile" => "test/data/female-167-70_100-bagwalk-10-1-g.txt"}}, "device"=>{"rate" => "100", "steps"=>"10", "trial"=>"one", "method"=>"run"}, "user"=>{"gender"=>"female", "height"=>"157", "stride"=>"90"}}
    
    assert_equal 200, last_response.status
  end

end