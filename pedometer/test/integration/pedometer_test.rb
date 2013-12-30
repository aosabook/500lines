require './pedometer.rb'
require 'test/unit'
require 'rack/test'

class PedometerTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_metrics_no_params
    get '/metrics'
    assert_equal '{"steps":0,"distance":0.0}', last_response.body
  end

  def test_metrics_with_params
    input = "x,y,z;0.123,-0.123,5;"
    get '/metrics', :data => input
    assert_equal '{"steps":1,"distance":0.0009}', last_response.body
  end

  def test_metrics_with_bad_params
    flunk
  end
end