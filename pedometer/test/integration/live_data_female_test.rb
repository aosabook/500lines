require 'test/unit'
require 'rack/test'
require_relative '../../pedometer'

class LiveDataFemaleTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_walk
    get '/trial/test/data/female-167-70_100-10-1-walk-a.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/female-167-70_100-10-2-walk-a.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/female-167-70_100-10-1-walk-g.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/female-167-70_100-10-2-walk-g.txt'
    assert_equal 200, last_response.status
  end

  def test_run
    get '/trial/test/data/female-167-70_100-10-1-run-a.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/female-167-70_100-10-2-run-a.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/female-167-70_100-10-1-run-g.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/female-167-70_100-10-2-run-g.txt'
    assert_equal 200, last_response.status
  end

  def test_bagwalk
    get '/trial/test/data/female-167-70_100-10-1-bagwalk-a.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/female-167-70_100-10-2-bagwalk-a.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/female-167-70_100-10-1-bagwalk-g.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/female-167-70_100-10-2-bagwalk-g.txt'
    assert_equal 200, last_response.status
  end

end