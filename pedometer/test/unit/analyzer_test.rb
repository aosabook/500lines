require 'test/unit'
require_relative '../../models/analyzer'

class AnalyzerTest < Test::Unit::TestCase

  PROCESSOR_MESSAGE = 'Processor invalid.'
  USER_MESSAGE   = 'User invalid.'
  DEVICE_MESSAGE = 'Device invalid.'

  # -- Creation Tests -------------------------------------------------------

  def test_create_combined_data
    processor = Processor.new('0.123,-0.123,5;')
    user   = User.new
    device = Device.new
    
    analyzer = Analyzer.new(processor, user, device)
    
    assert_equal processor, analyzer.processor
    assert_equal user,   analyzer.user
    assert_equal device, analyzer.device

    assert_nil analyzer.steps
    assert_nil analyzer.distance
    assert_nil analyzer.time
  end

  def test_create_separated_data
    processor = Processor.new('0.028,-0.072,5|0.129,-0.945,-5;')
    user   = User.new
    device = Device.new

    analyzer = Analyzer.new(processor, user, device)
    
    assert_equal processor, analyzer.processor
    assert_equal user,   analyzer.user
    assert_equal device, analyzer.device

    assert_nil analyzer.steps
    assert_nil analyzer.distance
    assert_nil analyzer.time
  end

  def test_create_no_processor
    assert_raise_with_message(RuntimeError, PROCESSOR_MESSAGE) do
      Analyzer.new(nil)
    end
  end

  def test_create_no_user_no_device
    processor = Processor.new('0.123,-0.123,5;')
    analyzer = Analyzer.new(processor)
    assert analyzer.user.kind_of? User
    assert analyzer.device.kind_of? Device
  end

  def test_create_bad_processor
    assert_raise_with_message(RuntimeError, PROCESSOR_MESSAGE) do
      Analyzer.new('bad processor')
    end
  end

  def test_create_bad_user
    assert_raise_with_message(RuntimeError, USER_MESSAGE) do
      processor = Processor.new('0.123,-0.123,5;')
      analyzer = Analyzer.new(processor, 'bad user')
    end
  end

  def test_create_bad_device
    assert_raise_with_message(RuntimeError, DEVICE_MESSAGE) do
      processor = Processor.new('0.123,-0.123,5;')
      analyzer = Analyzer.new(processor, User.new, 'bad device')
    end
  end

  # -- Edge Detection Tests -------------------------------------------------

  def test_count_edges
    processor = Processor.new(File.read('test/data/female-167-70_100-10-1-walk-g.txt'))
    analyzer = Analyzer.new(processor)
    
    assert_equal 9, analyzer.send(:count_edges, true)
    assert_equal 7, analyzer.send(:count_edges, false)
  end

  def test_count_edges_false_step
    processor = Processor.new(File.read('test/data/female-167-70_100-0-1-walk-g.txt'))
    analyzer = Analyzer.new(processor)
    
    assert_equal 1, analyzer.send(:count_edges, true)
    assert_equal 1, analyzer.send(:count_edges, false)
  end

  # -- Measurement Tests ----------------------------------------------------

  def test_measure
    user = User.new(nil, nil, 100)
    processor = Processor.new(File.read('test/data/female-167-70_100-10-1-walk-g.txt'))
    analyzer = Analyzer.new(processor, user)
    analyzer.measure

    assert_equal 8,          analyzer.steps
    assert_equal 800,        analyzer.distance
    assert_equal (1037/100), analyzer.time
  end

end