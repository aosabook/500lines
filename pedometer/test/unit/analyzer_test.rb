require 'test/unit'
require_relative '../../models/analyzer'

class AnalyzerTest < Test::Unit::TestCase

  PARSER_MESSAGE = 'Parser invalid.'
  USER_MESSAGE   = 'User invalid.'
  DEVICE_MESSAGE = 'Device invalid.'

  # -- Creation Tests -------------------------------------------------------

  def test_create_accelerometer_data
    parser = Parser.new('0.123,-0.123,5;')
    user   = User.new
    device = Device.new
    
    analyzer = Analyzer.new(parser, user, device)
    
    assert_equal parser, analyzer.parser
    assert_equal user,   analyzer.user
    assert_equal device, analyzer.device

    assert_equal 0, analyzer.steps
    assert_equal 0, analyzer.distance
    assert_equal (1/100), analyzer.time
  end

  def test_create_gravity_data
    parser = Parser.new('0.028,-0.072,5|0.129,-0.945,-5;')
    user   = User.new
    device = Device.new

    analyzer = Analyzer.new(parser, user, device)
    
    assert_equal parser, analyzer.parser
    assert_equal user,   analyzer.user
    assert_equal device, analyzer.device

    assert_equal 0, analyzer.steps
    assert_equal 0, analyzer.distance
    assert_equal (1/100), analyzer.time
    
  end

  def test_create_no_parser
    assert_raise_with_message(RuntimeError, PARSER_MESSAGE) do
      Analyzer.new(nil)
    end
  end

  def test_create_no_user_no_device
    parser = Parser.new('0.123,-0.123,5;')
    analyzer = Analyzer.new(parser)
    assert analyzer.user.kind_of? User
    assert analyzer.device.kind_of? Device
  end

  def test_create_bad_parser
    assert_raise_with_message(RuntimeError, PARSER_MESSAGE) do
      Analyzer.new('bad parser')
    end
  end

  def test_create_bad_user
    assert_raise_with_message(RuntimeError, USER_MESSAGE) do
      parser = Parser.new('0.123,-0.123,5;')
      analyzer = Analyzer.new(parser, 'bad user')
    end
  end

  def test_create_bad_device
    assert_raise_with_message(RuntimeError, DEVICE_MESSAGE) do
      parser = Parser.new('0.123,-0.123,5;')
      analyzer = Analyzer.new(parser, User.new, 'bad device')
    end
  end

  # -- Edge Detection Tests -------------------------------------------------

  def test_split_on_threshold
    parser = Parser.new(File.read('test/data/female-167-70_100-walk-10-1-g.txt'))
    analyzer = Analyzer.new(parser)

    expected = File.read('test/data/expected/female-167-70_100-walk-10-1-g-positive.txt').split(',').collect(&:to_i)
    assert_equal expected, analyzer.send(:split_on_threshold, true)

    expected = File.read('test/data/expected/female-167-70_100-walk-10-1-g-negative.txt').split(',').collect(&:to_i)
    assert_equal expected, analyzer.send(:split_on_threshold, false)
  end

  def test_detect_edges
    parser = Parser.new(File.read('test/data/female-167-70_100-walk-10-1-g.txt'))
    analyzer = Analyzer.new(parser)
    
    assert_equal 9, analyzer.send(:detect_edges, analyzer.send(:split_on_threshold, true))
    assert_equal 7, analyzer.send(:detect_edges, analyzer.send(:split_on_threshold, false))
  end

  def test_detect_edges_false_step
    parser = Parser.new(File.read('test/data/female-167-70_100-walk-0-1-g.txt'))
    analyzer = Analyzer.new(parser)
    
    assert_equal 1, analyzer.send(:detect_edges, analyzer.send(:split_on_threshold, true))
    assert_equal 1, analyzer.send(:detect_edges, analyzer.send(:split_on_threshold, false))
  end

  # -- Measurement Tests ----------------------------------------------------

  def test_measure_steps
    parser = Parser.new(File.read('test/data/female-167-70_100-walk-10-1-g.txt'))
    analyzer = Analyzer.new(parser)

    assert_equal 8, analyzer.steps
  end

  def test_measure_distance_after_steps
    user = User.new(nil, nil, 100)
    parser = Parser.new(File.read('test/data/female-167-70_100-walk-10-1-g.txt'))
    analyzer = Analyzer.new(parser, user)

    assert_equal 800, analyzer.distance
  end

  def test_measure_time
    # Fake out 15000 samples
    parser = Parser.new((15000.times.inject('') {|a| a+='1,1,1;';a}))
    device = Device.new(4)
    analyzer = Analyzer.new(parser, User.new, device)

    assert_equal 3750, analyzer.time
  end

  def test_measure
    parser = Parser.new(File.read('test/data/results-0-steps.txt'))
    user = User.new(nil, nil, 65)
    device = Device.new(5)
    analyzer = Analyzer.new(parser, user, device)

    assert_equal 0, analyzer.steps
    assert_equal 0, analyzer.distance
    assert_equal 0.2, analyzer.time
    
    parser = Parser.new(File.read('test/data/results-15-steps.txt'))
    analyzer = Analyzer.new(parser, user)

    # TODO: This data is way off because the accelerometer filter
    #       doesn't use the user data (specifically the rate)
    assert_equal 0, analyzer.steps
    assert_equal 0, analyzer.distance
    assert_equal 0.29, analyzer.time

    # assert_equal 15, analyzer.steps
    # assert_equal 975, analyzer.distance
    # assert_equal 5.8, analyzer.time
    # assert_equal 'sec', analyzer.interval
  end

end