require './models/user.rb'

class Pedometer

  CAP = 1.2

  attr_reader :raw_data, :parsed_data, :user
  attr_reader :steps, :distance, :time, :interval

  def initialize(mobile_output, user = nil)
    @raw_data = mobile_output
    @steps    = 0
    @distance = 0
    @time     = 0
    @interval = 'seconds'
    @user     = (user.kind_of? User) ? user : User.new

    verify_data_format
    parse_raw_data
  end

  # -- Measurement Methods --------------------------------------------------

  # TODO: Introduce user object passed in to:
  # - Request info in metric vs. imperial
  # - Request info in different reporting formats

  def measure
    measure_steps
    measure_distance
    measure_time
  end

  def measure_steps
    @parsed_data.each do |x, y, z|
      @steps += 1 if (x > CAP || y > CAP || z > CAP)
    end
  end

  def measure_distance
    @distance = @user.stride * @steps
  end

  def measure_time
    # TODO: Get sampling rate from user object
    sampling_rate = 5.0 # samples/second
    seconds = @parsed_data.count/sampling_rate

    if seconds > 3600
      @time = (seconds/3600).round(2)
      @interval = 'hours'
    elsif seconds > 60
      @time = (seconds/60).round(2)
      @interval = 'minutes'
    else
      @time = seconds.round(2)
      @interval = 'seconds'
    end
  end

  # -- Data Manipulation ----------------------------------------------------

  def verify_data_format
    regexp = Regexp.new(/^((-?\d+(?:\.\d+)?,){2}-?\d+(?:\.\d+)?;)+$/)
    unless regexp.match(@raw_data)
      raise "Bad Input. Ensure data is a series of comma separated x,y,z coordiantes separated by semicolons."
    end
  end

  def parse_raw_data
    @parsed_data = @raw_data.split(';').inject([]) do |a, row|
      a << row.split(',').map { |coord| coord.to_f.abs }
      a
    end
  end

end