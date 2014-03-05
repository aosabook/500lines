require 'mathn'
require './models/user.rb'
require './models/parser.rb'

class Analyzer

  CAP = 1.2

  attr_reader :parser, :user, :steps, :distance, :time, :time_interval, :distance_interval

  def initialize(parser, user = nil)
    unless (@parser = parser).kind_of? Parser
      raise "A Parser object must be passed in."
    end
    
    @steps = 0
    @distance = 0
    @time = 0
    @time_interval = 'sec'
    @distance_interval = 'cm'
    @user = (user.kind_of? User) ? user : User.new
  end

  # -- Edge Detection -------------------------------------------------------

  def split_on_threshold(positive)
    @parser.filtered_data.inject([]) do |a, data|
      a << (positive ? ((data < 0.2) ? 0 : 1) : ((data < -0.2) ? 1 : 0))
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

    if @distance > 99999
      @distance = (@distance/100000).round(2)
      @distance_interval = 'km'
    elsif @distance > 99
      @distance = (@distance/100).round(2)
      @distance_interval = 'm'
    end
  end

  def measure_time
    sampling_rate = @parser.device.rate.round(1)
    @time = @parser.parsed_data.count/sampling_rate

    if @time > 3600
      @time = @time/3600
      @time_interval = 'hours'
    elsif @time > 60
      @time = @time/60
      @time_interval = 'minutes'
    end

    @time = @time.round(2)
  end

end