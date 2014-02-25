require './pedometer.rb'
require 'test/unit'
require 'rack/test'

class PedometerTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_metrics
    get '/metrics', :device => { :data => "0.123,-0.123,5;", :rate => 4}, :user => {:stride => 90}
    
    assert_equal 200, last_response.status
    assert_equal '{"steps":0,"distance":0.0,"time":"0.25 seconds"}', last_response.body
  end

  def test_metrics_no_params
    get '/metrics'

    expected = 'Bad Input. Ensure accelerometer or gravity data is properly formatted.'
    assert_equal 400, last_response.status
    assert_equal expected, last_response.body
  end

  def test_metrics_no_user_param
    get '/metrics', :device => {:data => "0.123,-0.123,5;"}

    assert_equal 200, last_response.status
    assert_equal '{"steps":0,"distance":0.0,"time":"0.01 seconds"}', last_response.body
  end

  def test_metrics_no_data_param
    get '/metrics', :user => {:stride => 90}

    expected = 'Bad Input. Ensure accelerometer or gravity data is properly formatted.'
    assert_equal 400, last_response.status
    assert_equal expected, last_response.body
  end  

  def test_metrics_bad_data_param
    get '/metrics', :device => {:data => 'bad data'}, :user => {:stride => 90}

    expected = 'Bad Input. Ensure accelerometer or gravity data is properly formatted.'
    assert_equal 400, last_response.status
    assert_equal expected, last_response.body
  end

  def test_metrics_bad_user_param
    get '/metrics', :device => {:data => "0.123,-0.123,5;"}, :user => 'bad user'

    assert_equal 200, last_response.status
    assert_equal '{"steps":0,"distance":0.0,"time":"0.01 seconds"}', last_response.body
  end

  def test_data
    get '/data'

    assert_equal 200, last_response.status
  end

  def test_detail
    get '/detail/test/data/female/bag-10-a-1.txt'
    
    assert_equal 200, last_response.status
  end

end