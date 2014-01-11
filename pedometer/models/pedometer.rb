require './models/user.rb'
require 'mathn'

class Pedometer

  CAP = 1.2

  @format
  attr_reader :raw_data, :parsed_data, :dot_product_data, :filtered_data, :user
  attr_reader :steps, :distance, :time, :interval

  # TODO: Create new data class for data manipulation. 
  #       This class should take an instance of the data class
  #       and know how to return steps, distance, etc.
  
  def initialize(data, user = nil)
    @raw_data = data
    @steps    = 0
    @distance = 0
    @time     = 0
    @interval = 'seconds'
    @user     = (user.kind_of? User) ? user : User.new

    verify_data_format
    parse_raw_data
    dot_product_parsed_data
    filter_dot_product_data
  end

  # -- Measurement ----------------------------------------------------------

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
    regexp_accl = Regexp.new('^((' + any_decimal + ',){2}' + 
                                              any_decimal + ';)+$')
    regexp_grav = Regexp.new('^(((' + any_decimal + ',){2}(' +
                                         any_decimal + ')){1}[|](' +
                                         any_decimal + ',){2}(' +
                                         any_decimal + ';){1})+$')
    @format = 1 if regexp_accl.match(@raw_data)
    @format = 2 if regexp_grav.match(@raw_data)
    raise "Bad Input. Ensure accelerometer or gravity data is properly formatted." unless @format
  end

  def parse_raw_data
    case @format
    when 1
      @parsed_data = @raw_data.split(';').inject([]) do |a, data|
        a << data.split(',').map { |coord| coord.to_f.abs }
        a
      end
    when 2
      @parsed_data = @raw_data.split(';').inject([]) do |a, data|
        accl, grav = data.split('|')
        accl = accl.split(',')
        grav = grav.split(',')

        a << {:x => accl[0].to_f, :y => accl[1].to_f, :z => accl[2].to_f,
              :xg => grav[0].to_f, :yg => grav[1].to_f, :zg => grav[2].to_f}
        a
      end
    end    
  end

  def dot_product_parsed_data
    return unless @format == 2

    @dot_product_data = @parsed_data.inject([]) do |a, data|
      a << data[:x]*data[:xg] + data[:y]*data[:yg] + data[:z]*data[:zg]
      a
    end
  end

  # TODO: What about a0?
  # Chebyshev II, Astop = 2, Fstop = 5, Fs = 100
  # a0 = 1, a1 = -1.80898117793047, a2 = 0.827224480562408, 
  # b0 = 0.095465967120306, b1 = -0.172688631608676, b2 = 0.095465967120306
  def filter_dot_product_data
    return unless @format == 2

    a0 = 1
    a1 = -1.80898117793047
    a2 = 0.827224480562408
    b0 = 0.095465967120306
    b1 = -0.172688631608676
    b2 = 0.095465967120306

    @dot_product_data.length.times do |i|
      if i < 2
        @filtered_data ||= []
        @filtered_data << 0
      else
        @filtered_data << @dot_product_data[i]*b0 + 
                          @dot_product_data[i-1]*b1 + 
                          @dot_product_data[i-2]*b2 -
                          @filtered_data[i-1]*a1 -
                          @filtered_data[i-2]*a2
      end
    end
  end

end