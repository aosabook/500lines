require 'test/unit'
require 'rack/test'
require_relative '../../pedometer'

class LiveDataMaleTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_walk
    get '/trial/test/data/male-180-78_100-walk-10-1-a.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/male-180-78_100-walk-10-2-a.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/male-180-78_100-walk-10-1-g.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/male-180-78_100-walk-10-2-g.txt'
    assert_equal 200, last_response.status
  end

  def test_run
    get '/trial/test/data/male-180-78_100-run-10-1-a.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/male-180-78_100-run-10-2-a.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/male-180-78_100-run-10-1-g.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/male-180-78_100-run-10-2-g.txt'
    assert_equal 200, last_response.status
  end

  def test_bagwalk
    get '/trial/test/data/male-180-78_100-bagwalk-10-1-a.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/male-180-78_100-bagwalk-10-1-g.txt'
    assert_equal 200, last_response.status

    get '/trial/test/data/male-180-78_100-bagwalk-10-2-g.txt'
    assert_equal 200, last_response.status
  end

end