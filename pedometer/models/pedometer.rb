require 'mathn'
require './models/user.rb'
require './models/device_data.rb'

class Pedometer

  CAP = 1.2

  attr_reader :device_data, :user, :steps, :distance, :time, :interval

  def initialize(input_data, user = nil)
    unless input_data.kind_of? DeviceData
      raise "Input data must be of type DeviceData." 
    end
    
    @device_data = input_data
    @steps    = 0
    @distance = 0
    @time     = 0
    @interval = 'seconds'
    @user     = (user.kind_of? User) ? user : User.new
  end

  # -- Edge Detection -------------------------------------------------------

  def split_on_threshold_positive
    @device_data.filtered_data.inject([]) do |a, data|
      a << ((data < @user.threshold) ? 0 : 1)
      a
    end
  end

  def split_on_threshold_negative
    @device_data.filtered_data.inject([]) do |a, data|
      a << ((data < -@user.threshold) ? 1 : 0)
      a
    end
  end

  # TODO: Don't increase the counter if edges are too close together.
  #       Determine this using "if two steps are less than x seconds apart"
  #       using the rate, and x as a fraction of second
  def detect_edges(split)
    count = 0
    split.each_with_index do |data, i|
      count += 1 if (data == 1 && split[i-1] == 0)
    end
    count
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
    if @device_data.filtered_data
      steps_positive = detect_edges(split_on_threshold_positive)
      steps_negative = detect_edges(split_on_threshold_negative)
      
      @steps = ((steps_negative + steps_positive)/2).to_f.round
    else
      # TODO: Fix this. Bad algorithm.
      @device_data.parsed_data.each do |x, y, z|
        @steps += 1 if (x > CAP || y > CAP || z > CAP)
      end
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