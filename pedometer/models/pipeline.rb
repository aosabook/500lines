require_relative 'user'
require_relative 'trial'
require_relative 'parser'
require_relative 'processor'
require_relative 'analyzer'

class Pipeline

  attr_reader :user, :trial, :parser, :processor, :analyzer

  def initialize(file_path, user = User.new, trial = Trial.new)
    @user = user
    @trial = trial
    @parser = Parser.run(File.read(file_path))
    @processor = Processor.run(@parser.parsed_data)
    @analyzer = Analyzer.run(@processor.filtered_data, user, trial)
  end

end