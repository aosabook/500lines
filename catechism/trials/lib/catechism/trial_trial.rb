require 'catechism'

describe Catechism::Trial do
  it 'runs a trial file' do
    trial_fixture_path = File.expand_path('../../../fixtures/trials/dummy_trial.rb', __FILE__)
    trial = Catechism::Trial.new(trial_fixture_path)
    expect { trial.run }.to_send(:puts).with('All trials passed.').to(trial)
  end
end