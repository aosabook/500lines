require './pedometer.rb'
require 'test/unit'
require 'rack/test'

class LiveDataFemaleTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_gravity_walking_10_steps_1
    get '/metrics', :data => File.read('test/data/female/walking-10-g-1.txt'), :user => {:stride => 90, :rate => 100}

    assert_equal 200, last_response.status
    assert_equal '{"steps":8,"distance":720.0,"time":"10.37 seconds"}', last_response.body
  end

  def test_gravity_walking_10_steps_2
    get '/metrics', :data => File.read('test/data/female/walking-10-g-2.txt'), :user => {:stride => 90, :rate => 100}

    assert_equal 200, last_response.status
    assert_equal '{"steps":8,"distance":720.0,"time":"9.81 seconds"}', last_response.body
  end

  def test_gravity_jogging_10_steps_1
    get '/metrics', :data => File.read('test/data/female/jogging-10-g-1.txt'), :user => {:stride => 90, :rate => 100}

    assert_equal 200, last_response.status
    assert_equal '{"steps":10,"distance":900.0,"time":"9.39 seconds"}', last_response.body
  end

  def test_gravity_jogging_10_steps_2
    get '/metrics', :data => File.read('test/data/female/jogging-10-g-2.txt'), :user => {:stride => 90, :rate => 100}

    assert_equal 200, last_response.status
    assert_equal '{"steps":11,"distance":990.0,"time":"9.48 seconds"}', last_response.body
  end

  def test_gravity_bag_10_steps_1
    get '/metrics', :data => File.read('test/data/female/bag-10-g-1.txt'), :user => {:stride => 90, :rate => 100}

    assert_equal 200, last_response.status
    assert_equal '{"steps":9,"distance":810.0,"time":"9.31 seconds"}', last_response.body
  end

  def test_gravity_bag_10_steps_2
    get '/metrics', :data => File.read('test/data/female/bag-10-g-2.txt'), :user => {:stride => 90, :rate => 100}

    assert_equal 200, last_response.status
    assert_equal '{"steps":10,"distance":900.0,"time":"10.54 seconds"}', last_response.body
  end

end