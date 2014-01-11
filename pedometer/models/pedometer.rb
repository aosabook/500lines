require './models/user.rb'
require 'mathn'

class Pedometer

  CAP = 1.2

  @format
  attr_reader :raw_data, :parsed_data, :user
  attr_reader :steps, :distance, :time, :interval

  def initialize(data, user = nil)
    @raw_data = data
    @steps    = 0
    @distance = 0
    @time     = 0
    @interval = 'seconds'
    @user     = (user.kind_of? User) ? user : User.new

    verify_data_format
    parse_raw_data
  end
  
  # -- Measurement  ---------------------------------------------------------

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
    sampling_rate = @user.rate.round(1)
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
    # TODO: Can we clean this up? 
    any_decimal = '-?\d+(?:\.\d+)?'
    regexp_accelerometer = Regexp.new('^((' + any_decimal + ',){2}' + 
                                              any_decimal + ';)+$')
    regexp_gravity = Regexp.new('^(((' + any_decimal + ',){2}(' +
                                         any_decimal + ')){1}[|](' +
                                         any_decimal + ',){2}(' +
                                         any_decimal + ';){1})+$')
    @format = 1 if regexp_accelerometer.match(@raw_data)
    @format = 2 if regexp_gravity.match(@raw_data)
    raise "Bad Input. Ensure accelerometer or gravity data is properly formatted." unless @format
  end

  def parse_raw_data
    case @format
    when 1
      @parsed_data = @raw_data.split(';').inject([]) do |a, row|
        a << row.split(',').map { |coord| coord.to_f.abs }
        a
      end
    when 2
      @parsed_data = @raw_data.split(';').inject([]) do |a, row|
        accl, grav = row.split('|')
        accl = accl.split(',')
        grav = grav.split(',')

        a << {:x => accl[0].to_f, :y => accl[1].to_f, :z => accl[2].to_f,
              :xg => grav[0].to_f, :yg => grav[1].to_f, :zg => grav[2].to_f}
        a
      end
    end    
  end

end