require 'test/unit'
require 'rack/test'
require_relative '../../pedometer'

class LiveDataFemaleTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_walk
    get '/upload/test/data/female-167-70_1-walk-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_2-walk-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_3-walk-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_4-walk-100-10.txt'
    assert_equal 200, last_response.status
  end

  def test_run
    get '/upload/test/data/female-167-70_1-run-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_2-run-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_3-run-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_4-run-100-10.txt'
    assert_equal 200, last_response.status
  end

  def test_bagwalk
    get '/upload/test/data/female-167-70_1-bagwalk-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_2-bagwalk-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_3-bagwalk-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_4-bagwalk-100-10.txt'
    assert_equal 200, last_response.status
  end

end