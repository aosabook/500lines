require 'fileutils'
require_relative 'analyzer'

include FileUtils::Verbose

class Upload

  UPLOAD_DIRECTORY = 'public/uploads/'

  attr_reader :file_path, :user_params, :trial_params

  def initialize(file_path)
    # TODO: Error checking on file path?
    @file_path = file_path
  end

  # -- Class Methods --------------------------------------------------------

  def self.file_path(analyzer)
    UPLOAD_DIRECTORY + "#{analyzer.user.gender}-" + 
                       "#{analyzer.user.height}-" + 
                       "#{analyzer.user.stride}_" +
                       "#{analyzer.trial.name.to_s.gsub(/\s+/, '')}-" + 
                       "#{analyzer.trial.rate}-" + 
                       "#{analyzer.trial.steps}-" +
                       "#{analyzer.trial.method}.txt"
  end

  def self.create(temp_file, analyzer)
    cp(temp_file, self.file_path(analyzer))
  end

  def self.find(file_path)
    self.new(file_path)
  end

  def self.all
    file_paths = Dir.glob(File.join(UPLOAD_DIRECTORY, "*"))
    file_paths.map { |file_path| self.new(file_path) }
  end

  # -- Instance Methods -----------------------------------------------------

  def user_params
    file_name.first.split('-')
  end

  def trial_params
    file_name.last.split('-')
  end

private

  def file_name
    @file_name ||= file_path.split('/').last.split('.txt').first.split('_')
  end

end