class Pedometer

  CAP = 1.2

  attr_reader :raw_data, :parsed_data
  attr_reader :steps, :distance

  def initialize(mobile_output)
    @raw_data = mobile_output
    @steps    = 0
    @distance = 0

    verify_data_format
    parse_raw_data
  end

  # -- Measurement Methods --------------------------------------------------

  # TODO: Introduce user object passed in to:
  # - Get stride length, height, etc.
  # - Request info in metric vs. imperial
  # - Request info in different reporting formats

  def measure
    measure_steps
    measure_distance    
  end

  def measure_steps
    @parsed_data.each do |x, y, z|
      @steps += 1 if (x > CAP || y > CAP || z > CAP)
    end
  end

  def measure_distance
    # TODO: Get stride length from user object
    stride_length = 0.0009 # average stride length in km
    @distance = stride_length * @steps
  end

  # -- Data Manipulation ----------------------------------------------------

  def verify_data_format
    regexp = Regexp.new(/^((-?\d+(?:\.\d+)?,){2}-?\d+(?:\.\d+)?;)+$/)
    unless regexp.match(@raw_data)
      raise "Bad Input. Ensure data is a series of comma separated x,y,z coordiantes separated by semi colons."
    end
  end

  def parse_raw_data
    @parsed_data = @raw_data.split(';').inject([]) do |a, row|
      a << row.split(',').map { |coord| coord.to_f.abs }
      a
    end
  end

end