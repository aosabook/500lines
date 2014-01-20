require 'mathn'
require './models/user.rb'
require './models/device_data.rb'

class Analyzer

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

  def split_on_threshold(positive)
    @device_data.filtered_data.inject([]) do |a, data|
      if positive
        a << ((data < @user.threshold) ? 0 : 1)
      else
        a << ((data < -@user.threshold) ? 1 : 0)
      end
      a
    end
  end

  # TODO: Count the number of false steps, 
  # and if too many are occurring, don't count 
  # any steps at all
  
  def detect_edges(split)
    # Determined by the rate divided by the 
    # maximum steps the user can take per second
    min_interval = (@user.rate/6.0).round
    
    count = 0
    index_last_step = 0
    split.each_with_index do |data, i|
      # If the current value is 1 and the previous was 0, AND the 
      # interval between now and the last time a step was counted is 
      # above the minimun threshold, count this as a step
      if (data == 1) && (split[i-1] == 0)
        next if index_last_step > 0 && (i-index_last_step) < min_interval
        count += 1
        index_last_step = i
      end
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
      edges_positive = detect_edges(split_on_threshold(true))
      edges_negative = detect_edges(split_on_threshold(false))
      
      @steps = ((edges_positive + edges_negative)/2).to_f.round
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