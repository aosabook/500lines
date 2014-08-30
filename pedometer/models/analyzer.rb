require 'mathn'
require_relative 'processor'
require_relative 'user'
require_relative 'trial'

class Analyzer

  MIN_AMPLITUDE    = 0.18
  MAX_AMPLITUDE    = 1.0
  MIN_SEC_PER_STEP = 0.17
  MAX_SEC_PER_STEP = 0.7

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

  def measure_steps
    @steps          = 0
    next_0_crossing = 0
    min_period = MIN_SEC_PER_STEP * @trial.rate
    max_period = MAX_SEC_PER_STEP * @trial.rate

    @processor.filtered_data.each_with_index do |data, i|
      next if i < next_0_crossing
      if (data >= MIN_AMPLITUDE) && (@processor.filtered_data[i-1] < MIN_AMPLITUDE)
        remaining_signal = @processor.filtered_data[i..@processor.filtered_data.length - 1]

        next_0_crossing = remaining_signal.find_index { |x| x < 0 } || 0
        next unless next_0_crossing > 0
        next_0_crossing += i
        previous_0_crossing = @processor.filtered_data[0, i].rindex { |x| x < 0 } || 0
        
        peak = @processor.filtered_data[previous_0_crossing..next_0_crossing]

        next if peak.find { |x| x > MAX_AMPLITUDE }
        next if peak.length < min_period
        next if peak.length > max_period # TODO: this is not yet in tests

        @steps += 1
      end
    end
  end

  def measure_distance
    @distance = @user.stride * @steps
  end

  def measure_time
    @time = @processor.filtered_data.count/@trial.rate
  end

end