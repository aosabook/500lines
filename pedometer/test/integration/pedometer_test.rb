require './pedometer.rb'
require 'test/unit'
require 'rack/test'

class PedometerTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_metrics
    get '/metrics', :data => "0.123,-0.123,5;"

    assert_equal 200, last_response.status
    assert_equal '{"steps":1,"distance":74,"time":"0.2 seconds"}', last_response.body
  end

  def test_metrics_no_params
    get '/metrics'

    expected = 'Bad Input. Ensure data is a series of comma separated x,y,z coordiantes separated by semicolons.'
    assert_equal 400, last_response.status
    assert_equal expected, last_response.body
  end

  def test_metrics_bad_input
    get '/metrics', :data => "bad input"

    expected = 'Bad Input. Ensure data is a series of comma separated x,y,z coordiantes separated by semicolons.'
    assert_equal 400, last_response.status
    assert_equal expected, last_response.body
  end
end