require 'mathn'
require_relative 'processor'
require_relative 'user'
require_relative 'trial'

class Analyzer

  THRESHOLD = 0.2
  MAX_STEPS_PER_SECOND = 6.0

  attr_reader :processor, :user, :trial, :steps, :distance, :time

  def initialize(processor, user = User.new, trial = Trial.new)
    raise 'Processor invalid.' unless processor.kind_of? Processor
    raise 'User invalid.'      unless user.kind_of? User
    raise 'Trial invalid.'     unless trial.kind_of? Trial

    @processor = processor
    @user      = user
    @trial     = trial
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
  def count_threshold_cross(positive)
    count           = 0
    index_last_step = 0
    threshold       = positive ? THRESHOLD : -THRESHOLD
    min_interval    = (@trial.rate/MAX_STEPS_PER_SECOND)

    @processor.filtered_data.each_with_index do |data, i|
      # If the current value >= the threshold, and the previous was < the threshold
      # AND the interval between now and the last time a step was counted is 
      # above the minimun threshold, count this as a step
      if (data >= threshold) && (@processor.filtered_data[i-1] < threshold)
        next if index_last_step > 0 && (i-index_last_step) < min_interval
        count += 1
        index_last_step = i
      end
    end
    count
  end

  # -- Measurement ----------------------------------------------------------

  def measure_steps
    peak_count = count_threshold_cross(true)
    trough_count = count_threshold_cross(false)
    
    @steps = ((peak_count + trough_count)/2).to_f.round
  end

  def measure_distance
    @distance = @user.stride * @steps
  end

  def measure_time
    @time = @processor.filtered_data.count/@trial.rate
  end

end