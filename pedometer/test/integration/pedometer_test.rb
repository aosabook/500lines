require 'test/unit'
require 'rack/test'
require_relative '../../pedometer'

class PedometerTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_trials
    get '/trials'

    assert_equal 200, last_response.status
  end

  def test_trial
    get '/trial/test/data/female-167-70_100-10-1-bagwalk-a.txt'
    
    assert_equal 200, last_response.status
  end

  def test_create
    post '/create', {
      "parser" => {"file_upload"=>{"tempfile" => "test/data/female-167-70_100-10-1-bagwalk-g.txt"}}, 
      "device" => {"rate" => "100", "steps"=>"10", "trial"=>"one", "method"=>"run"}, "user"=>{"gender"=>"female", "height"=>"157", "stride"=>"90"}
    }
    
    assert_equal 200, last_response.status
  end

end