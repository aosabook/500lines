require './pedometer.rb'
require 'test/unit'
require 'rack/test'

class LiveDataFemaleTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_gravity_walking_10_steps_1
    get '/metrics', :device => {:data => File.read('test/data/male/walking-10-g-1.txt'), :rate => 100}, :user => {:stride => 90}

    assert_equal 200, last_response.status
    assert_equal '{"steps":9,"distance":810.0,"time":"16.14 seconds"}', last_response.body
  end

  def test_gravity_walking_10_steps_2
    get '/metrics', :device => {:data => File.read('test/data/male/walking-10-g-2.txt'), :rate => 100}, :user => {:stride => 90}

    assert_equal 200, last_response.status
    assert_equal '{"steps":8,"distance":720.0,"time":"17.28 seconds"}', last_response.body
  end

  def test_gravity_jogging_10_steps_1
    get '/metrics', :device => {:data => File.read('test/data/male/jogging-10-g-1.txt'), :rate => 100}, :user => {:stride => 90}

    assert_equal 200, last_response.status
    assert_equal '{"steps":14,"distance":1260.0,"time":"13.89 seconds"}', last_response.body
  end

  def test_gravity_jogging_10_steps_2
    get '/metrics', :device => {:data => File.read('test/data/male/jogging-10-g-2.txt'), :rate => 100}, :user => {:stride => 90}

    assert_equal 200, last_response.status
    assert_equal '{"steps":14,"distance":1260.0,"time":"13.45 seconds"}', last_response.body
  end

  def test_gravity_bag_10_steps_1
    get '/metrics', :device => {:data => File.read('test/data/male/bag-10-g-1.txt'), :rate => 100}, :user => {:stride => 90}

    assert_equal 200, last_response.status
    assert_equal '{"steps":7,"distance":630.0,"time":"27.58 seconds"}', last_response.body
  end

  def test_gravity_bag_10_steps_2
    get '/metrics', :device => {:data => File.read('test/data/male/bag-10-g-2.txt'), :rate => 100}, :user => {:stride => 90}

    assert_equal 200, last_response.status
    assert_equal '{"steps":4,"distance":360.0,"time":"26.82 seconds"}', last_response.body
  end

end