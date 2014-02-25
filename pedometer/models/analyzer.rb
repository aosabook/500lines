require 'mathn'
require './models/user.rb'
require './models/parser.rb'

class Analyzer

  CAP = 1.2

  attr_reader :parser, :user, :steps, :distance, :time, :interval

  def initialize(parser, user = nil)
    unless (@parser = parser).kind_of? Parser
      raise "A Parser object must be passed in."
    end
    
    @steps    = 0
    @distance = 0
    @time     = 0
    @interval = 'seconds'
    @user     = (user.kind_of? User) ? user : User.new
  end

  # -- Edge Detection -------------------------------------------------------

  def split_on_threshold(positive)
    @parser.filtered_data.inject([]) do |a, data|
      if positive
        a << ((data < 0.2) ? 0 : 1)
      else
        a << ((data < -0.2) ? 1 : 0)
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
    min_interval = (@parser.device.rate/6.0).round
    
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
    edges_positive = detect_edges(split_on_threshold(true))
    edges_negative = detect_edges(split_on_threshold(false))
    
    @steps = ((edges_positive + edges_negative)/2).to_f.round
  end

  def measure_distance
    @distance = @user.stride * @steps
  end

  def measure_time
    sampling_rate = @parser.device.rate.round(1)
    seconds = @parser.parsed_data.count/sampling_rate

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