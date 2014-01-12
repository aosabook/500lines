require 'mathn'
require './models/user.rb'
require './models/device_data.rb'

class Pedometer

  CAP = 1.2

  attr_reader :device_data, :user, :steps, :distance, :time, :interval

  # TODO: Create new data class for data manipulation. 
  #       This class should take an instance of the data class
  #       and know how to return steps, distance, etc.
  
  def initialize(input_data, user = nil)
    unless input_data.kind_of? DeviceData
      raise "Input data must be of type DeviceData." 
    end
    
    @device_data = input_data
    @raw_data = input_data
    @steps    = 0
    @distance = 0
    @time     = 0
    @interval = 'seconds'
    @user     = (user.kind_of? User) ? user : User.new
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
    @device_data.parsed_data.each do |x, y, z|
      @steps += 1 if (x > CAP || y > CAP || z > CAP)
    end
  end

  def measure_distance
    @distance = @user.stride * @steps
  end

  def measure_time
    sampling_rate = @user.rate.round(1)
    seconds = @device_data.parsed_data.count/sampling_rate

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

end