require_relative 'upload'
require_relative 'parser'
require_relative 'processor'
require_relative 'analyzer'

class Pipeline

  attr_reader :upload, :parser, :processor, :analyzer

  def self.run(upload)
    pipeline = Pipeline.new(upload)
    pipeline.feed
    pipeline
  end

  def initialize(upload)
    @upload = upload
  end

  def feed
    @parser    = Parser.run(File.read(@upload.file_path))
    @processor = Processor.run(@parser.parsed_data)
    @analyzer  = Analyzer.run(@processor.filtered_data, @upload.user, @upload.trial)
  end

end