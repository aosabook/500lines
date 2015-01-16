require 'test/unit'
require 'rack/test'
require_relative '../../pedometer'

class LiveDataFemaleTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_walk
    get '/upload/test/data/female-167-70_walk1-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_walk2-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_walk3-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_walk4-100-10.txt'
    assert_equal 200, last_response.status
  end

  def test_run
    get '/upload/test/data/female-167-70_run1-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_run2-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_run3-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_run4-100-10.txt'
    assert_equal 200, last_response.status
  end

  def test_bagwalk
    get '/upload/test/data/female-167-70_bagwalk1-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_bagwalk2-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_bagwalk3-100-10.txt'
    assert_equal 200, last_response.status

    get '/upload/test/data/female-167-70_bagwalk4-100-10.txt'
    assert_equal 200, last_response.status
  end

end