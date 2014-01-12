require 'thor'

class Catechism::Cli < Thor
  desc 'run_trial TRIAL', 'Run the given Catechism file, optionally with line number'
  def run_trial(trial_parameter)
    trial_path, trial_line = trial_parameter.split(':')
    trial = Catechism::Trial.new(trial_path)
    if trial_line.nil?
      trial.run
    else
      trial.run_at_line(trial_line)
    end
  end

  desc 'run_all_trials', 'Run all trials in the trials directory under the current path'
  def run_all_trials
    Catechism::Suite.new(File.expand_path('trials')).run_all
  end

  default_task :run_all_trials
end
