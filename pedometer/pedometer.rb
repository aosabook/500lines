class Pedometer

  CAP = 1.2

  attr_reader :raw_data, :parsed_data
  attr_reader :steps, :distance

  def initialize(mobile_output)
    @raw_data = mobile_output
    @steps    = 0
    @distance = 0

    parse_raw_data
  end

  # TODO: Introduce user object passed in to:
  # - Get stride length, height, etc.
  # - Request info in metric vs. imperial
  # - Request info in different reporting formats
  def results
    count_steps
    count_distance
    
    { :steps => @steps, :distance => @distance }
  end

  # -- Counters -------------------------------------------------------------

  def count_steps
    @parsed_data.each do |x, y, z|
      @steps += 1 if (x > CAP || y > CAP || z > CAP)
    end
  end

  def count_distance
    # TODO: Get stride length from user object
    stride_length = 0.9 # average stride length in meters
    @distance = stride_length * @steps
  end

  # -- Parsers --------------------------------------------------------------

  def parse_raw_data
    rows = @raw_data.gsub('x,y,z;', '').split(';')
    @parsed_data = rows.inject([]) do |a, row|
      a << row.split(',').map { |coord| coord.to_f.abs }
      a
    end
  end

end