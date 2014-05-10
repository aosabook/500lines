require 'test/unit'
require_relative '../../models/trial'

class TrialTest < Test::Unit::TestCase

  def test_find
    trial = Trial.find('test/data/female-167-70_100-10-1-walk-g.txt')
    assert_equal File.read('test/data/female-167-70_100-10-1-walk-g.txt'), trial.data
    assert_equal ["female", "167", "70"], trial.user_params
    assert_equal ["100", "10", "1", "walk"], trial.device_params
  end

end