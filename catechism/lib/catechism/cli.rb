require 'thor'

class Catechism::Cli < Thor
  desc 'run_trial TRIAL', 'Run the given trial'
  def run_trial(trial_path)
    Kernel.system("bundle exec ruby #{trial_path}") && puts("#{trial_path} was successful.")
  end

  desc 'run_all_trials', 'Run all trials in the current path'
  def run_all_trials
    Dir.glob('trials/**/*_trial.rb') { |trial_path| run_trial(trial_path) }
  end

  default_task :run_all_trials
end
