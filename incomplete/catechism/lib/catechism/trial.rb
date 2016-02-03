class Catechism::Trial
  attr_reader :trial_path

  def initialize(trial_path)
    @trial_path = File.expand_path(trial_path)
    instance_eval(File.read(trial_path), trial_path)
  end

  def describe_blocks
    @describe_blocks ||= []
  end

  def describe(description, &block)
    describe_block = Catechism::DescribeBlock.new(description, block)
    describe_blocks << describe_block
  end

  def run
    describe_blocks.each(&:call)
    puts "All trials passed in #{trial_path}."
  end
end