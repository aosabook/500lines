require 'catechism'

describe Catechism::Trial do
  it 'runs a trial file' do
    trial_fixture_path = File.expand_path('../../../fixtures/trials/dummy_trial.rb', __FILE__)
    trial = Catechism::Trial.new(trial_fixture_path)
    expect { trial.run }.to_send(:puts).with("All trials passed in #{trial_fixture_path}.").to(trial)
  end

  it 'adds describe blocks' do
    trial_fixture_path = File.expand_path('../../../fixtures/trials/dummy_trial.rb', __FILE__)
    trial = Catechism::Trial.new(trial_fixture_path)
    expect(trial.describe_blocks.count).to_equal(1)
  end
end