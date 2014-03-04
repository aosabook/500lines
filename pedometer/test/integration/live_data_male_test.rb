require './pedometer.rb'
require 'test/unit'
require 'rack/test'

class LiveDataFemaleTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_walk
    get '/detail/test/data/male-180-78_100-walk-10-1-a.txt'
    assert_equal 200, last_response.status

    get '/detail/test/data/male-180-78_100-walk-10-2-a.txt'
    assert_equal 200, last_response.status

    get '/detail/test/data/male-180-78_100-walk-10-1-g.txt'
    assert_equal 200, last_response.status

    get '/detail/test/data/male-180-78_100-walk-10-2-g.txt'
    assert_equal 200, last_response.status
  end

  def test_run
    get '/detail/test/data/male-180-78_100-run-10-1-a.txt'
    assert_equal 200, last_response.status

    get '/detail/test/data/male-180-78_100-run-10-2-a.txt'
    assert_equal 200, last_response.status

    get '/detail/test/data/male-180-78_100-run-10-1-g.txt'
    assert_equal 200, last_response.status

    get '/detail/test/data/male-180-78_100-run-10-2-g.txt'
    assert_equal 200, last_response.status
  end

  def test_bagwalk
    get '/detail/test/data/male-180-78_100-bagwalk-10-1-a.txt'
    assert_equal 200, last_response.status

    get '/detail/test/data/male-180-78_100-bagwalk-10-1-g.txt'
    assert_equal 200, last_response.status

    get '/detail/test/data/male-180-78_100-bagwalk-10-2-g.txt'
    assert_equal 200, last_response.status
  end

end