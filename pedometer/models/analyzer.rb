require 'mathn'
require_relative 'user'
require_relative 'parser'

class Analyzer

  MAX_STEPS_PER_SECOND = 6.0
  THRESHOLD = 0.2

  attr_reader :parser, :user, :steps, :distance, :time

  def initialize(parser, user = User.new)
    raise "Parser invalid." unless parser.kind_of? Parser
    raise "User invalid." unless user.kind_of? User

    @parser = parser
    @user = user
    @steps = 0
    @distance = 0
    @time = 0

    # TODO: Call each measurement method from here
  end

  # -- Edge Detection -------------------------------------------------------

  def split_on_threshold(positive)
    # TODO: 
    # - Rewrite challenge
    # - Can this be combined with detect_edges?
    @parser.filtered_data.collect do |data|
      (positive ? ((data < THRESHOLD) ? 0 : 1) : ((data < -THRESHOLD) ? 1 : 0))
    end
  end

  # TODO: Count the number of false steps, 
  # and if too many are occurring, don't count 
  # any steps at all

  def detect_edges(split)
    # Determined by the rate divided by the 
    # maximum steps the user can take per second
    min_interval = (@parser.device.rate/MAX_STEPS_PER_SECOND).round
    
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

private

  # TODO: One method, rewrite
  def measure_steps
    edges_positive = detect_edges(split_on_threshold(true))
    edges_negative = detect_edges(split_on_threshold(false))
    
    @steps = ((edges_positive + edges_negative)/2).to_f.round
  end

  def measure_distance
    @distance = @user.stride * @steps
  end

  def measure_time
    sampling_rate = @parser.device.rate
    @time = @parser.parsed_data.count/sampling_rate
  end

end