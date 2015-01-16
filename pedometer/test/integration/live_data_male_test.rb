require 'test/unit'
require 'rack/test'
require_relative '../../pedometer'

class LiveDataMaleTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_walk
    get '/upload/test/data/male-180-78_walk1-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/male-180-78_walk2-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/male-180-78_walk1-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/male-180-78_walk2-100-10.txt'
    assert_equal 200, last_response.status
  end

  def test_run
    get '/upload/test/data/male-180-78_run1-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/male-180-78_run2-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/male-180-78_run1-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/male-180-78_run2-100-10.txt'
    assert_equal 200, last_response.status
  end

  def test_bagwalk
    get '/upload/test/data/male-180-78_bagwalk1-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/male-180-78_bagwalk2-100-10.txt'
    assert_equal 200, last_response.status
  end

end