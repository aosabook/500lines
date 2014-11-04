require_relative 'user'
require_relative 'trial'
require_relative 'parser'
require_relative 'processor'
require_relative 'analyzer'

class Pipeline

  attr_reader :file_path, :user, :trial, :parser, :processor, :analyzer

  def self.run(upload)
    pipeline = Pipeline.new(upload)
    pipeline.feed
    pipeline
  end

  def initialize(upload)
    @file_path = upload.file_path
    @user      = upload.user
    @trial     = upload.trial
  end

  def feed
    @parser    = Parser.run(File.read(@file_path))
    @processor = Processor.run(@parser.parsed_data)
    @analyzer  = Analyzer.run(@processor.filtered_data, @user, @trial)
  end

end