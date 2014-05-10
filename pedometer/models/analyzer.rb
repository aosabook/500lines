require 'mathn'
require_relative 'parser'
require_relative 'user'
require_relative 'device'

class Analyzer

  MAX_STEPS_PER_SECOND = 6.0
  THRESHOLD = 0.2

  attr_reader :parser, :user, :device, :steps, :distance, :time

  def initialize(parser, user = User.new, device = Device.new)
    raise "Parser invalid." unless parser.kind_of? Parser
    raise "User invalid."   unless user.kind_of? User
    raise "Device invalid." unless device.kind_of? Device

    @parser = parser
    @user   = user
    @device = device
  end

  def measure
    measure_steps
    measure_distance
    measure_time
  end

private

  # -- Edge Detection -------------------------------------------------------

  # TODO: 
  # - It would help to have an explanation of why we 
  # need to do edge detection here. What is an edge?
  # - Count the number of false steps, 
  # and if too many are occurring, don't count 
  # any steps at all
  def count_edges(positive)
    count           = 0
    index_last_step = 0
    threshold       = positive ? THRESHOLD : -THRESHOLD
    min_interval    = (@device.rate/MAX_STEPS_PER_SECOND)

    @parser.filtered_data.each_with_index do |data, i|
      # If the current value >= the threshold, and the previous was < the threshold
      # AND the interval between now and the last time a step was counted is 
      # above the minimun threshold, count this as a step
      if (data >= threshold) && (@parser.filtered_data[i-1] < threshold)
        next if index_last_step > 0 && (i-index_last_step) < min_interval
        count += 1
        index_last_step = i
      end
    end
    count
  end

  # -- Measurement ----------------------------------------------------------

  def measure_steps
    positive_edge_count = count_edges(true)
    negative_edge_count = count_edges(false)
    
    @steps = ((positive_edge_count + negative_edge_count)/2).to_f.round
  end

  def measure_distance
    @distance = @user.stride * @steps
  end

  def measure_time
    @time = @parser.filtered_data.count/@device.rate
  end

end